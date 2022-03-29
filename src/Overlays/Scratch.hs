{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Overlays.Scratch
  ( manage,
    keyMap,
  )
where

import Control.Arrow (first)
import Control.Monad (join, void)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C (CUInt)
import Graphics.X11
import qualified Graphics.X11 as X
import Graphics.X11.ExtraTypes
import Lib (ensureXDG)
import Lib.Actions (CmdRunner (createCmd, (./)), ProcessState, sh, zsh, (>$))
import Lib.Actions.Pipes (Pipe ((|>)), syscat, writeNull, (&>))
import Overlays.Base ((<//))
import qualified Overlays.Base as O
import qualified System.Directory as Dir
import System.Environment (lookupEnv)
import System.Process (CreateProcess (CreateProcess, cmdspec, std_err, std_out), StdStream (Inherit), createProcess)
import XMonad (MonadIO (liftIO), (.|.))
import qualified XMonad as XM
import XMonad.Core (spawn)
import qualified XMonad.Hooks.ManageHelpers as XM
import XMonad.ManageHook ((<&&>), (<+>), (=?))
import qualified XMonad.ManageHook as H
import qualified XMonad.StackSet as SS
import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as S

-- xdgStateHome = "${XDG_STATE_HOME:-$HOME/.local/state}"

-- scratchLogDir = xdgStateHome <> "/xmonad/scratch"

data PostCmd = Hold | Quit

type ProgramM pDef = (Text, pDef, XM.Query Bool)

type Program = ProgramM CreateProcess

type ProgramIO = ProgramM (IO CreateProcess)

progWithClass :: Text -> m -> ProgramM m
progWithClass className cmd = (className, cmd, H.className =? T.unpack className)

progWithName :: Text -> m -> ProgramM m
progWithName name cmd = (name, cmd, H.appName =? T.unpack name)

progSimpleC :: Text -> Program
progSimpleC cmd = progWithClass cmd $ cmd >$ []

progSimpleA :: Text -> Program
progSimpleA cmd = progWithName cmd $ cmd >$ []

progTerm :: Text -> [Text] -> Program
progTerm name termArgs = (name, "kitty" >$ (["--class", name] <> termArgs), H.appName =? T.unpack name <&&> H.className =? T.unpack name)

editCmd name args = "neovide" >$ (["--multigrid", "--x11-wm-class", name] <> args)

editQuery name = H.appName =? "neovide" <&&> H.className =? T.unpack name

progEdit :: Text -> [Text] -> Program
progEdit name args = (name, editCmd name args, editQuery name)

progEditIO :: Text -> IO [Text] -> ProgramIO
progEditIO name args = (name, editCmd name <$> args, editQuery name)

progSudoEdit :: Text -> [Text] -> Program
progSudoEdit name args = progTerm name (["sudo", "nvim"] <> args)

progCmd :: Text -> [Text] -> PostCmd -> Program
progCmd name cmd post = progTerm name $ case post of
  Hold -> "--hold" : "--" : cmd
  Quit -> "--" : cmd

progShll :: [Text] -> Program
progShll (cmd : args) = progCmd cmd (cmd : args) Quit

data Browser = Firefox | Qute

brsXApp :: Browser -> Text
brsXApp Firefox = "Navigator"
brsXApp Qute = "qutebrowser"

instance CmdRunner Browser (Text, Text, Maybe Text) where
  createCmd Firefox (instDir, inst, url) =
    ("firefox-nightly" :: Text)
      ./ (["-P", inst, "--new-instance", "--class=" <> inst] <> maybeToList url)
  createCmd Qute (instDir, inst, url) =
    ("qutebrowser" :: Text)
      ./ (["--basedir", instDir, "--qt-arg", "class", inst] <> maybeToList url)

instance CmdRunner Browser (Text, Maybe Text) where
  createCmd Firefox (inst, url) = ("firefox-nightly" :: Text) ./ (["-P", inst, "--new-instance", "--class=" <> inst] <> maybeToList url)
  createCmd Qute (inst, url) = zsh ./ mconcat ["qutebrowser --basedir ${XDG_STATE_HOME:-$HOME/.local/state}/", inst, " --qt-arg class ", inst, maybe "" (" " <>) url]

instance CmdRunner Browser Text where
  createCmd Firefox url = ("firefox-nightly" :: Text) ./ url
  createCmd Qute url = ("qutebrowser" :: Text) ./ url

progSSB :: Browser -> Text -> Maybe Text -> ProgramIO
progSSB brs inst url =
  ( inst,
    do
      (stateDir :: Text) <- fromString <$> ensureXDG Dir.XdgState inst
      return $ brs ./ (stateDir, inst, url),
    H.appName =? T.unpack (brsXApp brs) <&&> H.className =? T.unpack inst
  )

class ToManageHook mh where
  toManageHook :: mh -> XM.ManageHook

instance ToManageHook XM.ManageHook where
  toManageHook = id

instance ToManageHook W.RationalRect where
  toManageHook = S.customFloating

instance ToManageHook (Rational, Rational) where
  toManageHook (width, height) = toManageHook $ W.RationalRect ((1 - width) / 2) ((1 - height) / 2) width height

instance ToManageHook Rational where
  toManageHook size = toManageHook (size, size)

class ToScratchpad sp where
  toSP :: sp -> S.NamedScratchpad

instance ToScratchpad S.NamedScratchpad where
  toSP = id

runProg :: Text -> CreateProcess -> IO [ProcessState]
runProg name proc = do
  (procs :: [CreateProcess]) <- (proc, std_out proc == Inherit, std_err proc == Inherit) |> syscat name
  sequence $
    ( \procDef -> do
        putStrLn $ "createProg: " <> show (cmdspec procDef)
        createProcess procDef
    )
      <$> procs

instance (ToManageHook tmh) => ToScratchpad (Text, ProgramM (IO CreateProcess), tmh) where
  toSP (name, (pName, procIO :: IO CreateProcess, query), hook) =
    S.NS
      (T.unpack name)
      (liftIO . void $ runProg name =<< procIO)
      query
      (toManageHook hook)

instance (ToManageHook tmh) => ToScratchpad (Text, ProgramM CreateProcess, tmh) where
  toSP (name, (pName, procDef :: CreateProcess, query), hook) =
    S.NS
      (T.unpack name)
      (liftIO . void $ runProg name procDef)
      query
      (toManageHook hook)

spBig :: Rational
spBig = 7 / 8

spSmall :: Rational
spSmall = 2 / 3

spMsg :: XM.ManageHook
spMsg =
  let width = spBig
      height = (spBig / 2)
   in XM.doRectFloat (W.RationalRect ((1 - width) / 2) ((1 - spBig) / 2) width height) <+> XM.doF SS.focusDown

instance (ToScratchpad (Text, ProgramM m, Rational)) => ToScratchpad (Text, ProgramM m) where
  toSP (name, prog) = toSP (name, prog, spBig)

instance (ToManageHook tmh, ToScratchpad (Text, ProgramM m, tmh)) => ToScratchpad (ProgramM m, tmh) where
  toSP (prog@(name, _, _), hook) = toSP (name, prog, hook)

instance (ToScratchpad (Text, ProgramM m, Rational)) => ToScratchpad (ProgramM m) where
  toSP prog@(name, _, _) = toSP (name, prog, spBig)

data IntoScratchpad = forall a. ToScratchpad a => IntoScratchpad a

instance ToScratchpad IntoScratchpad where
  toSP (IntoScratchpad sp) = toSP sp

type KeyBind = (X.ButtonMask, X.KeySym)

class ToKeybind b where
  toKB :: b -> X.ButtonMask -> KeyBind

instance ToKeybind KeyBind where
  toKB b mask = b

instance ToKeybind (X.ButtonMask -> X.ButtonMask, X.KeySym) where
  toKB (mFn, sym) mask = (mFn mask, sym)

instance ToKeybind (X.ButtonMask -> KeyBind) where
  toKB b = b

instance ToKeybind X.KeySym where
  toKB sym mask = (mask .|. X.shiftMask, sym)

infixr 5 .:.

(.:.) :: (ToScratchpad sp, ToKeybind kb) => (sp, kb) -> [ScratchBind] -> [ScratchBind]
(sp, kb) .:. sbs = (toSP sp, toKB kb) : sbs

type ScratchBind = (S.NamedScratchpad, X.ButtonMask -> KeyBind)

pads :: [ScratchBind]
pads =
  -- term
  (progTerm "scratch_term" [], (sysMI, xK_grave))
    -- meta
    .:. (progCmd "scratch_rexmonad" xmRecompileCmd Hold, \sysM -> (sysM .|. altM .|. shiftM, xK_x))
    .:. (progCmd "scratch_update" upgradeCmd Hold, xK_u)
    .:. (progShll ["btop"], xK_t)
    .:. ((progTerm "scratch_logs" ["journalctl", "-fe"], spMsg), \sysM -> (sysM .|. altM, xK_grave))
    .:. (progSSB Qute "activitywatch" $ Just "http://localhost:5600/#/home", \sysM -> (sysM .|. X.mod1Mask, xF86XK_Calculator))
    .:. ((progSimpleA "blueman-manager", spSmall), xK_b)
    -- editors
    .:. (progEdit "edit_main" [], (sysMI, xK_e))
    .:. ( progEditIO "edit_notes" $ (: []) <$> getProjectPath "notes/main.norg",
          xK_n
        )
    .:. (progEditIO "edit_dot" $ Dir.getHomeDirectory >>= (\home -> return [home <> "/dotfiles/README.md"]) . fromString, xK_c)
    .:. (progEditIO "edit_xmonad" $ Dir.getXdgDirectory Dir.XdgConfig "xmonad" >>= (\xmcfg -> return [xmcfg <> "/app/Main.hs"]) . fromString, xK_x)
    -- IM
    .:. (progWithName "discord" ((writeNull >>= \null -> return $ "discord-canary" >$ [] &> null) :: IO CreateProcess), xK_d)
    .:. (progWithName "element" $ "element-desktop" >$ [], xK_m)
    .:. (progSimpleA "slack", xK_o)
    .:. (progTerm "scratch_irc" ["weechat"], xK_i)
    -- phone
    .:. ((progSimpleA "kdeconnect-sms", spSmall), (altM, xF86XK_Calculator))
    .:. ((progSimpleA "kdeconnect-app", spSmall), (ctrlM .|. altM, xF86XK_Calculator))
    .:. ((progWithName "scrcpy" $ "scrcpy" >$ ["--power-off-on-close", "-SKd"], S.defaultFloating), \sysM -> (sysM .|. ctrlM, xF86XK_Calculator))
    -- internet
    .:. (progSimpleC "firefox-nightly", xK_f)
    .:. (progSimpleC "thunderbird-nightly", xK_e)
    -- media
    .:. (progSSB Firefox "scratch_media" Nothing, xK_y)
    .:. ((progWithName "io.github.quodlibet.QuodLibet" $ "quodlibet" >$ [], spSmall), xK_w)
    .:. ((progSimpleA "pavucontrol", spSmall), xK_v)
    .:. (progSimpleA "qpwgraph", ((shiftM .|. ctrlM .|.), xK_v))
    -- util
    .:. ((progSimpleA "kalgebra", S.defaultFloating), (noneM, xF86XK_Calculator))
    .:. ((progWithName "bitwarden" $ "bitwarden-desktop" >$ [], spSmall), xK_p)
    .:. ((progWithName "authy desktop" $ "authy" >$ [], S.defaultFloating), xK_a)
    .:. (progWithName "github desktop" $ "github-desktop" >$ [], xK_g)
    .:. []
  where
    sysMI :: X.ButtonMask -> X.ButtonMask
    sysMI = id
    sysKB :: X.KeySym -> X.ButtonMask -> KeyBind
    sysKB sym sysM = (sysM, sym)
    noneM = 0 :: X.ButtonMask
    ctrlM = X.controlMask
    shiftM = X.shiftMask
    altM = X.mod1Mask
    numM = X.mod2Mask
    supM = X.mod3Mask
    hypM = X.mod4Mask
    xmRecompileCmd = ["sh", "-c", "xmonad --recompile && xmonad --restart && notify-send 'XMonad restarted'"]
    upgradeCmd = ["sh", "-c", "sudo nix profile upgrade .\\* && notify-send 'Root Nix updated' && nix profile upgrade .\\* && notify-send 'User Nix updated; beginning yay -Syu' && yay -Syu && notify-send 'Update Complete'"]
    getProjectPath :: Text -> IO Text
    getProjectPath path = do
      home <- Dir.getHomeDirectory
      (\pdir -> if path == "" then pdir else pdir <> "/" <> path) . fromString . fromMaybe home <$> lookupEnv "ASH_PROJECT_DIR"

padDefs :: [S.NamedScratchpad]
padDefs = fst <$> pads

manage :: XM.ManageHook
manage = S.namedScratchpadManageHook padDefs

keyMap :: XM.XConfig XM.Layout -> Map.Map (XM.ButtonMask, X.KeySym) (XM.X ())
keyMap conf@(XM.XConfig {XM.modMask = sysM}) = Map.fromList $ toKBWithAction <$> pads
  where
    pMod = sysM
    padAction :: String -> XM.X ()
    padAction = S.namedScratchpadAction padDefs
    toKBWithAction :: ScratchBind -> ((XM.ButtonMask, X.KeySym), XM.X ())
    toKBWithAction (dfn, kb) = (kb sysM, padAction $ S.name dfn)

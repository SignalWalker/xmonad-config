{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Overlays.Scratch
  ( manage,
    keyMap,
  )
where

import Control.Arrow (first)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.X11
import qualified Graphics.X11 as X
import Graphics.X11.ExtraTypes
import Overlays.Base ((<//))
import qualified Overlays.Base as O
import XMonad ((.|.))
import qualified XMonad as XM
import XMonad.Core (spawn)
import XMonad.ManageHook ((=?))
import qualified XMonad.ManageHook as H
import qualified XMonad.StackSet as W
import qualified XMonad.Util.NamedScratchpad as S

xdgStateHome = "${XDG_STATE_HOME:-$HOME/.local/state}"

scratchLogDir = xdgStateHome <> "/xmonad/scratch"

logPipe :: Text -> Text
logPipe name = " &> " <> scratchLogDir <> "/" <> name <> ".log"

type KeyBind = (X.ButtonMask, X.KeySym)

class ToKeybind b where
  toKB :: b -> X.ButtonMask -> KeyBind

instance ToKeybind KeyBind where
  toKB b mask = b

instance ToKeybind (X.ButtonMask -> KeyBind) where
  toKB b mask = b mask

instance ToKeybind X.KeySym where
  toKB sym mask = (mask .|. X.shiftMask, sym)

data PostCmd = Hold | Quit

type Program = (XM.X (), XM.Query Bool)

program cmd query = (cmd, query)

progWithClass :: XM.X () -> Text -> Program
progWithClass cmd className = program cmd $ H.className =? T.unpack className

progSimple :: Text -> Program
progSimple cmd = progWithClass (spawn $ T.unpack cmd) cmd

progTerm :: Text -> [Text] -> Program
progTerm name termArgs = progWithClass (T.intercalate " " $ ["${ASH_TERMINAL:-kitty}", "--class", name] <> termArgs) name

progEdit :: Text -> Text -> Program
progEdit name trg = progWithClass ("neovide --multigrid --x11-wm-class " <> name <> " -- " <> trg) name

progCmd name cmd post = progTerm name $ case post of
  Hold -> ["--hold", "--", cmd]
  Quit -> ["--", cmd]

progShll cmd = progCmd cmd cmd Quit

progEnvOr :: Text -> Program -> Program
progEnvOr var (defCmd, query) = (T.concat ["${", T.toUpper var, ":-", defCmd, "}"], query)

progSSB :: Text -> Text -> Program
progSSB inst url = ("qutebrowser -B " <> xdgStateHome <> "/" <> inst <> " --qt-arg name " <> inst <> " -- '" <> url, H.appName =? T.unpack inst)

class ToProgram pr where
  toProg :: pr -> Program

instance ToProgram Program where
  toProg = id

instance ToProgram (Text, Text) where
  toProg (cmd, className) = (cmd, H.className =? T.unpack className)

instance ToProgram Text where
  toProg cmd = toProg (cmd, cmd)

instance ToProgram (Text, [Text]) where
  toProg (name, args) = toProg (T.intercalate " " $ ["${ASH_TERMINAL:-kitty}", "--class", name] ++ args, name)

instance ToProgram (Text, Text, PostCmd) where
  toProg (name, cmd, post) =
    toProg
      ( name,
        case post of
          Hold -> ["--hold", "--", cmd]
          Quit -> ["--", cmd]
      )

type ScratchBind = (S.NamedScratchpad, X.ButtonMask -> KeyBind)

spBig :: Rational
spBig = 7 / 8

spSmall :: Rational
spSmall = 2 / 3

class ToManageHook mh where
  toManageHook :: mh -> XM.ManageHook

instance ToManageHook XM.ManageHook where
  toManageHook = id

instance ToManageHook W.RationalRect where
  toManageHook rect = S.customFloating rect

instance ToManageHook (Rational, Rational) where
  toManageHook (width, height) = toManageHook $ W.RationalRect ((1 - width) / 2) ((1 - height) / 2) width height

instance ToManageHook Rational where
  toManageHook size = toManageHook (size, size)

class ToScratchpad sp where
  toSP :: sp -> S.NamedScratchpad

instance ToScratchpad S.NamedScratchpad where
  toSP = id

instance (ToManageHook tmh) => ToScratchpad (Text, Program, tmh) where
  toSP (name, (prog, query), hook) =
    S.NS
      (T.unpack name)
      (spawn $ T.unpack prog)
      query
      (toManageHook hook)

instance ToScratchpad (Text, Program) where
  toSP (name, prog) = toSP (name, prog, spBig)

instance (ToManageHook tmh) => ToScratchpad (Program, tmh) where
  toSP (prog@(cmd, _), hook) = toSP (cmd, prog, hook)

instance ToScratchpad Program where
  toSP prog@(cmd, _) = toSP (cmd, prog, spBig)

pads :: [ScratchBind]
pads =
  [ -- terminal
    (toSP $ progTerm "scratch_term" [], sysKB xK_grave),
    (toSP $ progCmd "scratch_rexmonad" xmRecompileCmd Hold, toKB $ \sysM -> (sysM .|. X.mod1Mask .|. X.shiftMask, xK_x)),
    -- (toSP $ padCmd ("scratch_logs", ""), KeySet (X.mod1Mask .|. X.shiftMask, xK_l))
    (toSP $ progCmd "scratch_update" upgradeCmd Hold, toKB xK_u),
    (toSP $ progShll "btop", toKB xK_t)
  ]
    ++ [ -- editors
         (toSP $ progEdit "edit_main" "$HOME", sysKB xK_e),
         (toSP $ progEdit "edit_notes" "${ASH_PROJECT_DIR:-$HOME/projects}/notes/main.norg", toKB xK_n),
         (toSP $ progEdit "edit_dot" "$HOME/dotfiles", toKB xK_c),
         (toSP $ progEdit "edit_xmonad" "$HOME/.config/xmonad/flake.nix", toKB xK_x)
       ]
    ++ [ -- apps
         (toSP $ progEnvOr "ASH_BROWSER" $ progSimple "firefox-nightly", toKB xK_f),
         (toSP $ progEnvOr "ASH_DISCORD" $ progWithClass "discord-canary" "discord", toKB xK_d),
         (toSP $ progEnvOr "ASH_SLACK" $ progWithClass "slack" "Slack", toKB xK_o),
         (toSP $ progEnvOr "ASH_EMAIL" $ progSimple "thunderbird-nightly", toKB xK_e),
         (toSP $ (progEnvOr "ASH_MUSIC" $ progSimple "cantata", spSmall), toKB xK_w),
         (toSP $ (progEnvOr "ASH_KEYCHAIN" $ progWithClass "bitwarden-desktop" "Bitwarden", spSmall), toKB xK_p),
         (toSP $ (progEnvOr "ASH_2FA" $ progWithClass "authy" "Authy Desktop", S.defaultFloating), toKB xK_a),
         (toSP $ progEnvOr "ASH_MATRIX" $ progWithClass "element-desktop" "Element", toKB xK_m),
         (toSP $ (progEnvOr "ASH_VOLUME" $ progWithClass "pavucontrol" "Pavucontrol", spSmall), toKB xK_v),
         -- (toSP $ (progWithClass "cherry_tomato" "CherryTomato", spSmall), toKB (maskNone, xF86XK_Calculator)),
         (toSP $ progSSB "activitywatcher" "http://localhost:5600/#/home", toKB (maskNone, xF86XK_Calculator)),
         (toSP $ progWithClass "kdeconnect-app" "kdeconnect.app", toKB $ \sysM -> (sysM .|. X.mod1Mask, xF86XK_Calculator)),
         (toSP $ progWithClass "gitkraken" "GitKraken", toKB xK_g)
       ]
  where
    maskNone = 0 :: X.ButtonMask
    sysKB sym sysM = (sysM, sym)
    xmRecompileCmd = "sh -c 'xmonad --recompile && xmonad --restart'"
    upgradeCmd = "sh -c 'sudo nix profile upgrade .\\* && nix profile upgrade .\\* && notify-send \"Beginning yay -Syu\" && yay -Syu && notify-send \"Update Complete\"'"

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

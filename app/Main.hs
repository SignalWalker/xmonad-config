{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.X11.ExtraTypes.XF86
import qualified Hooks as H
import Lib (ensureXConfig, getXConfig)
import Lib.Actions (ProcessState, fish, sh, zsh, (./), (>$))
import Lib.Actions.Pipes (Forkable (fork), Pipe, (|&>))
import qualified Overlays as O
import qualified Overlays.Keymap as K
import qualified Overlays.Scratch as S
import qualified System.Directory as Dir
import System.IO (IOMode (WriteMode))
import System.Process (createProcess, readCreateProcess, readProcess, shell)
import XMonad (focusFollowsMouse, keys, layoutHook, manageHook, modMask, startupHook, terminal, workspaces, xmonad, (<+>))
import qualified XMonad as XM
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.Hooks.ManageDocks as DO
import qualified XMonad.Hooks.StatusBar as SB
import qualified XMonad.Hooks.StatusBar.PP as PP
import qualified XMonad.Hooks.UrgencyHook as UH
import qualified XMonad.Util.Cursor as CU
import qualified XMonad.Util.NamedScratchpad as NS
import qualified XMonad.Util.WorkspaceCompare as WS

-- import qualified System.Log.Simple as Log

cfg =
  XM.def
    { terminal = "kitty",
      focusFollowsMouse = False,
      modMask = XM.mod4Mask,
      workspaces = map show ([1 .. 9] <> [0]) <> [NS.scratchpadWorkspaceTag],
      keys = S.keyMap <+> K.keyMap,
      layoutHook = H.layout,
      startupHook = do
        liftIO $ putStrLn "startup hook..."
        -- modMap <- liftIO $ readCreateProcess ("sh" >$ ["xmodmap", "-pm"]) ""
        -- liftIO $ putStrLn $ "modmap: " <> modMap
        CU.setDefaultCursor CU.xC_left_ptr
        -- autostartDir <- liftIO $ ensureXConfig "autostart"
        -- liftIO $ createProcess $ "dex" >$ ["-as", fromString autostartDir]
        fork $ sh ./ ("feh --force-aliasing --bg-fill --recursive --randomize $(xdg-user-dir PICTURES)/backgrounds" :: Text)
        fork $ "kitty" >$ []
        liftIO $ putStrLn "startup complete",
      manageHook = H.manage <+> S.manage
    }

main :: IO ()
main =
  xmonad
    . UH.withUrgencyHook UH.NoUrgencyHook
    . Ewmh.setEwmhActivateHook UH.doAskUrgent
    . Ewmh.addEwmhWorkspaceSort (pure $ WS.filterOutWs ["NSP"])
    . Ewmh.ewmhFullscreen
    . Ewmh.ewmh
    . SB.dynamicEasySBs statusBar
    . K.navi (XM.modMask cfg)
    $ cfg
  where
    pcfgIO = getXConfig "polybar/config.ini"
    polybar :: FilePath -> XM.ScreenId -> String -> String
    polybar cfg (XM.S scr) bar =
      mconcat ["systemd-cat --identifier='polybar::", show scr, "' env MONITOR=$(polybar -m | sed -n '", show (scr + 1), "{s/:.*$//g;p;q}') polybar -cr ", cfg, " ", bar, " & disown"]
    statusBar' :: XM.ScreenId -> String -> IO SB.StatusBarConfig
    statusBar' scr@(XM.S scrn) bar = do
      pcfg <- pcfgIO
      putStrLn $ mconcat ["polybar :: ", pcfg, " :: scr ", show scrn, " : bar ", bar]
      let cmd = polybar pcfg scr bar
      putStrLn $ mconcat ["    cmd: " <> cmd]
      return $ SB.statusBarGeneric cmd mempty
    statusBar :: XM.ScreenId -> IO SB.StatusBarConfig
    statusBar 0 = statusBar' 0 "primary"
    statusBar scr = statusBar' scr "secondary"

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.X11.ExtraTypes.XF86
import qualified Hooks as H
import Lib (Workspaces, envLogFile, getLogFile, logDir, mainLogFile, writeLog)
import Lib.Actions (ProcessState, createProc, sh, (./))
import Lib.Actions.Pipes (Pipe, (&>))
import qualified Overlays as O
import qualified Overlays.Keymap as K
import qualified Overlays.Scratch as S
import System.IO (IOMode (WriteMode))
import System.Process (createProcess, shell)
import XMonad (focusFollowsMouse, keys, layoutHook, manageHook, modMask, startupHook, terminal, workspaces, xmonad, (<+>))
import qualified XMonad as XM
import qualified XMonad.Hooks.DynamicLog as DL
import qualified XMonad.Hooks.EwmhDesktops as Ewmh
import qualified XMonad.Hooks.ManageDocks as DO
import qualified XMonad.Hooks.StatusBar as SB
import qualified XMonad.Hooks.StatusBar.PP as PP
import qualified XMonad.Hooks.UrgencyHook as UH
import qualified XMonad.Util.Cursor as CU
import qualified XMonad.Util.WorkspaceCompare as WS

-- import qualified System.Log.Simple as Log

mkCfg logFile =
  XM.def
    { terminal = "kitty",
      focusFollowsMouse = False,
      modMask = XM.mod3Mask,
      workspaces = map show ([1 .. 9] <> [0]) <> ["NSP"],
      keys = S.keyMap <+> K.keyMap,
      layoutHook = DO.avoidStruts H.layout,
      startupHook =
        mconcat $
          CU.setDefaultCursor CU.xC_left_ptr :
          ( void . XM.xfork . void
              <$> [ ((sh ./ "date && ${XDG_CONFIG_HOME:-$HOME/.config}/polybar/launch.sh") &> writeLog "polybar.log") >>= createProcess,
                    ((sh ./ "date && feh --force-aliasing --bg-fill --recursive --randomize $HOME/pictures/backgrounds") &> writeLog "feh.log") >>= createProcess
                  ]
          ),
      manageHook = H.manage <+> S.manage
    }

main :: IO ()
main = do
  logFile <- mainLogFile
  let cfg = mkCfg logFile
  xmonad
    . Ewmh.setEwmhActivateHook UH.doAskUrgent
    . Ewmh.addEwmhWorkspaceSort (pure $ WS.filterOutWs ["NSP"])
    . Ewmh.ewmhFullscreen
    . Ewmh.ewmh
    . DL.xmobarProp
    . UH.withUrgencyHook UH.NoUrgencyHook
    . K.navi (XM.modMask cfg)
    $ cfg

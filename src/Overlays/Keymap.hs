{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Overlays.Keymap
  ( keyMap,
    navi,
  )
where

import Data.Function (on)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Graphics.X11
import qualified Graphics.X11 as X
import Graphics.X11.ExtraTypes
import Lib (Workspaces)
import Lib.Actions
import Overlays.Base (Mergeable, Overlay, Overlay', (<//))
import System.Exit (exitSuccess)
import XMonad (spawn, (.|.))
import qualified XMonad as XM
import qualified XMonad.Actions.FloatKeys as FL
import qualified XMonad.Actions.Navigation2D as Nav
import qualified XMonad.StackSet as Stack
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.Ungrab (unGrab)

-- overlay :: Overlay'
-- overlay conf = (navi ) {XM.keys = XM.keys conf <// keyMap}

navi (sysM :: XM.ButtonMask) =
  Nav.navigation2D
    Nav.def
    (xK_k, xK_h, xK_j, xK_l)
    [(sysM, Nav.windowGo), (sysM .|. shiftMask, Nav.windowSwap)]
    False

toggleFloat win =
  XM.windows
    ( \stk ->
        if Map.member win (Stack.floating stk)
          then Stack.sink win stk
          else Stack.float win (Stack.RationalRect (1 / 16) (1 / 16) (7 / 8) (7 / 8)) stk
    )

keyMap :: XM.XConfig XM.Layout -> Map.Map (XM.ButtonMask, X.KeySym) (XM.X ())
keyMap conf@(XM.XConfig {XM.modMask = sysM, XM.terminal = term}) =
  let altM = mod1Mask
      ctrlM = controlMask
      shiftM = shiftMask

      xmM = sysM .|. altM
      appM = sysM .|. shiftM
   in Map.fromList $
        [ -- XMonad Control
          ((xmM .|. ctrlM, xK_q), XM.io exitSuccess)
        ]
          ++ [ -- Layout Control
               ((sysM .|. shiftM, xK_space), XM.withFocused toggleFloat),
               ((xmM, xK_space), XM.sendMessage XM.NextLayout),
               ((xmM .|. ctrlM, xK_space), XM.setLayout $ XM.layoutHook conf), -- reset
               ((xmM, xK_r), XM.refresh)
             ]
          ++ [ -- Display Control
               ((xmM, xK_l), spawn "i3lock -e -i $HOME/pond_bg_black.png"),
               -- brightness
               ((0, xF86XK_MonBrightnessUp), spawn "light -A 5"),
               ((0, xF86XK_MonBrightnessDown), spawn "light -U 5"),
               ((ctrlM, xF86XK_MonBrightnessUp), spawn "light -S 100"),
               ((ctrlM, xF86XK_MonBrightnessDown), spawn "light -S 100")
             ]
          ++ [ -- Audio Control
               ((0, xF86XK_AudioPrev), spawn "playerctl -s previous"),
               ((0, xF86XK_AudioPlay), spawn "playerctl -s play-pause"),
               ((0, xF86XK_AudioNext), spawn "playerctl -s next"),
               ((0, xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ +2%"),
               ((0, xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume @DEFAULT_SINK@ -2%"),
               ((0, xF86XK_AudioMute), spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle"),
               ((altM, xF86XK_AudioRaiseVolume), spawn "pactl set-source-volume @DEFAULT_SOURCE@ +2%"),
               ((altM, xF86XK_AudioLowerVolume), spawn "pactl set-source-volume @DEFAULT_SOURCE@ -2%"),
               ((0, xF86XK_AudioMicMute), spawn "pactl set-source-mute @DEFAULT_SOURCE@ toggle")
             ]
          ++ [ -- Applications
               -- general
               ((sysM, xK_Return), spawn $ XM.terminal conf),
               ((sysM, xK_d), unGrab *> spawn "rofi -show drun"),
               ((appM, xK_q), XM.withFocused XM.killWindow), -- kill individual window (or whole app, if former not supported)
               ((appM .|. altM, xK_q), XM.kill), -- kill whole app
               -- display
               ((0, xK_Print), unGrab *> spawn (maim [])),
               ((appM, xK_s), unGrab *> spawn (maim ["-us"])),
               -- notifications
               ((sysM .|. altM, xK_n), spawn "dunstctl history-pop"),
               ((sysM, xK_n), spawn "dunstctl close")
               -- system
               -- ((xmM, xK_x), XM.withFocused )
             ]
          ++ [ -- Workspaces
               ((sysM .|. modifier, key), XM.windows $ fn workspace)
               | (workspace, key) <- zip (XM.workspaces conf) $ [xK_1 .. xK_9] ++ [xK_0, xK_minus],
                 (fn, modifier) <- [(Stack.greedyView, 0), (Stack.shift, shiftM)]
             ]
  where
    (-|-) :: Text -> Text -> Text
    a -|- b = a <> " | " <> b
    previewImgCmd = "timeout 3 feh --class img_preview -. -"
    xclipImgCmd = "xclip -sel clip -t image/png -f" -|- previewImgCmd
    maim args = T.unpack $ (T.intercalate " " $ ["maim"] <> args) -|- xclipImgCmd

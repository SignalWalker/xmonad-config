module Hooks.Layouts
  ( hook
  )
where

import qualified XMonad as XM
import XMonad ((|||))


hook = tiled ||| XM.Mirror tiled ||| XM.Full
  where
    tiled = XM.Tall nmain delta ratio
    nmain = 1 -- Default number of windows in the main pane
    ratio = 1 / 2 -- Default proportion of screen occupied by main pane
    delta = 3 / 100 -- Percent of screen to increment by when resizing panes

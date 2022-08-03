module Hooks.Layouts
  ( hook,
  )
where

import XMonad ((|||))
import qualified XMonad as XM
import qualified XMonad.Layout.Minimize as Min
import qualified XMonad.Layout.NoBorders as NB
import qualified XMonad.Layout.Dishes as Dish
import XMonad.Layout.SimplestFloat (simplestFloat)

hook = Min.minimize (tiled ||| XM.Mirror tiled ||| NB.smartBorders XM.Full)
  where
    tiled = NB.smartBorders $ XM.Tall nmain delta ratio
    nmain = 1 -- Default number of windows in the main pane
    ratio = 1 / 2 -- Default proportion of screen occupied by main pane
    delta = 2 / 100 -- Percent of screen to increment by when resizing panes

module Hooks
  ( manage,
    layout,
  )
where

import qualified Hooks.Layouts as L
import XMonad ((-->), (<+>), (=?))
import qualified XMonad as XM
import XMonad.Hooks.ManageHelpers ((-?>))
import qualified XMonad.Hooks.ManageHelpers as XM
import qualified XMonad.StackSet as SS

layout = L.hook

manage :: XM.ManageHook
manage =
  XM.composeOne $
    [ XM.isKDETrayWindow -?> XM.doIgnore,
      XM.transience,
      XM.isDialog -?> XM.doFloat
    ]
      ++ [ XM.className =? "img_preview" -?> (cornerFloat <+> noFocus)
         ]
  where
    noFocus = XM.doF SS.focusDown
    cornerFloat = XM.doRectFloat $ SS.RationalRect (3 / 4) (3 / 4) (1 / 4) (1 / 4)

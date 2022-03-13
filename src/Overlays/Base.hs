{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Overlays.Base
  ( Overlay,
    Overlay',
    Mergeable,
    (<//),
  )
where

import qualified Data.Map as Map
import XMonad ((<+>))
import qualified XMonad as XM

type Overlay l r = XM.XConfig l -> XM.XConfig r

type Overlay' = Overlay XM.Layout XM.Layout

class Mergeable m n o | m n -> o where
  (<//) :: m -> n -> o

instance (Ord k) => Mergeable (Map.Map k v) (Map.Map k v) (Map.Map k v) where
  a <// b = Map.union b a

type Fn x y = x -> y

instance (Mergeable y y y) => Mergeable (Fn x y) (Fn x y) (Fn x y) where
  f <// g = \i -> f i <// g i

-- instance (Mergeable b c d) => Mergeable (Fn a b) (Fn a c) (Fn a d) where
--   f <// g = \i -> f i <// g i

instance Mergeable XM.ManageHook XM.ManageHook XM.ManageHook where
  a <// b = b <+> a

-- instance (Mergeable l) => Mergeable (XConfig l) where
--   a <// b =
--     a
--       { normalBorderColor = b normalBorderColor,
--         focusedBorderColor = b focusedBorderColor,
--         terminal = b terminal,
--         layoutHook = b layoutHook,
--         manageHook = b manageHook <+> a manageHook
--       }
--       b

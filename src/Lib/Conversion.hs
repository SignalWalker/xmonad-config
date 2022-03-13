{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib.Conversion
  ( From,
    from,
    Into,
    into,
  )
where

import Data.Text (Text, unpack)
import Data.Type.Equality (type (==))

class Into t f where
  into :: f -> t
  (>/) :: f -> t
  (>/) f = into f

class From f t where
  from :: f -> t

instance (From f t) => Into t f where
  into = from

instance From f f where
  from = id

-- instance ((a == b) ~ False, (b == c) ~ False, From a b, From b c) => From a c where
--   from a = (from :: b -> c) . (from :: a -> b)

instance From f [f] where
  from f = [f]

instance (From f t, Functor fn) => From (fn f) (fn t) where
  from = fmap from

instance (Show str) => From str String where
  from = show

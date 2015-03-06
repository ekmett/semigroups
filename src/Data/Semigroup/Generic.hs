{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}
module Data.Semigroup.Generic
  ( GSemigroup
  , genericMappend
  ) where

import Control.Applicative
import Data.Semigroup 
import GHC.Generics

-- | Generically generate a 'Semigroup' ('<>') operation for any type
-- implementing 'Generic'. This operation will append two values
-- by point-wise appending their component fields. It is only defined
-- for product types.
genericMappend :: (Generic a, GSemigroup (Rep a)) => a -> a -> a
genericMappend x y = to (gmappend (from x) (from y))

class GSemigroup f where
  gmappend :: f p -> f p -> f p

instance GSemigroup U1 where
  gmappend _ _ = U1

instance GSemigroup V1 where
  gmappend x y = x `seq` y `seq` error "GSemigroup.V1: gmappend"

instance Semigroup a => GSemigroup (K1 i a) where
  gmappend (K1 x) (K1 y) = K1 (x <> y)

instance GSemigroup f => GSemigroup (M1 i c f) where
  gmappend (M1 x) (M1 y) = M1 (gmappend x y)

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
  gmappend (x1 :*: x2) (y1 :*: y2) = gmappend x1 y1 :*: gmappend x2 y2

instance (Applicative f, GSemigroup g) => GSemigroup (f :.: g) where
  gmappend (Comp1 m) (Comp1 n) = Comp1 (liftA2 gmappend m n)

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Monoid.Generic
  ( GMonoid
  , genericMappend
  , genericMempty
  ) where

import Data.Semigroup
import Data.Semigroup.Generic
import GHC.Generics
import Control.Applicative

-- | Generically generate a 'Monoid' 'mempty' for any product-like type
-- implementing 'Generic'.
--
-- It is only defined for product types.
--
-- @
-- 'genericMappend' 'genericMempty' a = a = 'genericMappend' a 'genericMempty'
-- @

genericMempty :: (Generic a, GMonoid (Rep a)) => a
genericMempty = to gmempty

class GSemigroup f => GMonoid f where
  gmempty :: f p

instance GMonoid U1 where
  gmempty = U1

instance (Semigroup a, Monoid a) => GMonoid (K1 i a) where
  gmempty = K1 mempty

instance GMonoid f => GMonoid (M1 i c f) where
  gmempty = M1 gmempty

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
  gmempty = gmempty :*: gmempty

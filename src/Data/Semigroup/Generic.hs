{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#else
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup.Generic
-- Copyright   :  (C) 2014-2015 Edward Kmett, Eric Mertens
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides generic deriving tools for monoids and semigroups for
-- product-like structures.
--
----------------------------------------------------------------------------
module Data.Semigroup.Generic
  ( GSemigroup
  , gmappend
  , GMonoid
  , gmempty
  ) where

import Data.Semigroup
import GHC.Generics

-- | Generically generate a 'Semigroup' ('<>') operation for any type
-- implementing 'Generic'. This operation will append two values
-- by point-wise appending their component fields. It is only defined
-- for product types.
--
-- @
-- 'gmappend' a ('gmappend' b c) = 'gmappend' ('gmappend' a b) c
-- @
gmappend :: (Generic a, GSemigroup (Rep a)) => a -> a -> a
gmappend x y = to (gmappend' (from x) (from y))

class GSemigroup f where
  gmappend' :: f p -> f p -> f p

instance GSemigroup U1 where
  gmappend' _ _ = U1

instance GSemigroup V1 where
  gmappend' x y = x `seq` y `seq` error "GSemigroup.V1: gmappend'"

instance Semigroup a => GSemigroup (K1 i a) where
  gmappend' (K1 x) (K1 y) = K1 (x <> y)

instance GSemigroup f => GSemigroup (M1 i c f) where
  gmappend' (M1 x) (M1 y) = M1 (gmappend' x y)

instance (GSemigroup f, GSemigroup g) => GSemigroup (f :*: g) where
  gmappend' (x1 :*: x2) (y1 :*: y2) = gmappend' x1 y1 :*: gmappend' x2 y2

-- | Generically generate a 'Monoid' 'mempty' for any product-like type
-- implementing 'Generic'.
--
-- It is only defined for product types.
--
-- @
-- 'gmappend' 'gmempty' a = a = 'gmappend' a 'gmempty'
-- @

gmempty :: (Generic a, GMonoid (Rep a)) => a
gmempty = to gmempty'

class GSemigroup f => GMonoid f where
  gmempty' :: f p

instance GMonoid U1 where
  gmempty' = U1

instance (Semigroup a, Monoid a) => GMonoid (K1 i a) where
  gmempty' = K1 mempty

instance GMonoid f => GMonoid (M1 i c f) where
  gmempty' = M1 gmempty'

instance (GMonoid f, GMonoid g) => GMonoid (f :*: g) where
  gmempty' = gmempty' :*: gmempty'

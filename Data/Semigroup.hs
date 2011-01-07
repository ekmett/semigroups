-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
----------------------------------------------------------------------------
module Data.Semigroup ( 
    Semigroup(..)
  , Min(..)
  , Max(..)
  , Option(..)
  , WrappedMonoid(..)
  ) where

import Prelude hiding (foldr1)
import Data.Monoid
import Data.Foldable

infixl 4 <> 

class Semigroup a where
  (<>) :: a -> a -> a

  fold1 :: Foldable f => f a -> a
  fold1 = foldr1 (<>) 

-- Semigroups from Data.Monoid

instance Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g) 

instance Semigroup All where
  All a <> All b = All (a && b)

instance Semigroup Any where
  Any a <> Any b = Any (a || b)

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)

instance Semigroup (First a) where
  First Nothing <> b = b
  a             <> _ = a

instance Semigroup (Last a) where
  a <> Last Nothing = a
  _ <> b            = b


newtype Min a = Min { getMin :: a } 
instance Ord a => Semigroup (Min a) where
  Min a <> Min b = Min (a `min` b)

newtype Max a = Max { getMax :: a } 
instance Ord a => Semigroup (Max a) where
  Max a <> Max b = Max (a `min` b)

-- (==)/XNOR on Bool forms a Semigroup, but has no good name


newtype WrappedMonoid m = WrapMonoid { unwrapMonoid :: m } 
  deriving (Show, Read, Eq, Ord)

instance Monoid m => Semigroup (WrappedMonoid m) where
  WrapMonoid a <> WrapMonoid b = WrapMonoid (a `mappend` b)

instance Monoid m => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty
  WrapMonoid a `mappend` WrapMonoid b = WrapMonoid (a `mappend` b)


newtype Option a = Option { runOption :: Maybe a } 
  deriving (Show, Read, Eq, Ord)

instance Semigroup a => Semigroup (Option a) where
  Option Nothing <> b = b
  a <> Option Nothing = a
  Option (Just a) <> Option (Just b) = Option (Just (a <> b))

instance Semigroup a => Monoid (Option a) where
  mempty = Option Nothing

  Option Nothing `mappend` b = b
  a `mappend` Option Nothing = a
  Option (Just a) `mappend` Option (Just b) = Option (Just (a <> b))

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
-- In mathematics, a semigroup is an algebraic structure consisting of a
-- set together with an associative binary operation. A semigroup
-- generalizes a monoid in that there might not exist an identity
-- element. It also (originally) generalized a group (a monoid with all
-- inverses) to a type where every element did not have to have an inverse,
-- thus the name semigroup.
--
-- The use of @(\<\>)@ in this module conflicts with an operator with the same
-- name that is being exported by Data.Monoid. However, this package
-- re-exports (most of) the contents of Data.Monoid, so to use semigroups
-- and monoids in the same package just
--
-- > import Data.Semigroup
--
----------------------------------------------------------------------------
module Data.Semigroup (
    Semigroup(..)
  -- * Semigroups
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , WrappedMonoid(..)
  -- * Re-exported monoids from Data.Monoid
  , Monoid(..)
  , Dual(..)
  , Endo(..)
  , All(..)
  , Any(..)
  , Sum(..)
  , Product(..)
  -- * A better monoid for Maybe
  , Option(..)
  , option
  -- * Difference lists of a semigroup
  , diff
  , cycle1
  ) where

import Prelude hiding (foldr1)
import Data.Monoid (Monoid(..),Dual(..),Endo(..),All(..),Any(..),Sum(..),Product(..),Endo(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Data.Monoid as Monoid
import Data.Foldable
import Data.Traversable
import Data.List.NonEmpty

import Numeric.Natural.Internal
import Data.Sequence (Seq, (><))
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.IntMap (IntMap)

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

infixr 6 <>

class Semigroup a where
  -- | An associative operation.
  --
  -- > (a <> b) <> c = a <> (b <> c)
  (<>) :: a -> a -> a

  -- | Reduce a non-empty list with @\<\>@
  --
  -- The default definition should be sufficient, but this can be overridden for efficiency.
  --
  sconcat :: NonEmpty a -> a
  sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b

  -- | Repeat a value (n + 1) times.
  --
  -- > times1p n a = a <> a <> ... <> a  -- using <> n times
  --
  -- The default definition uses peasant multiplication, exploiting associativity to only
  -- require /O(log n)/ uses of @\<\>@.

  times1p :: Whole n => n -> a -> a
  times1p y0 x0 = f x0 (1 Prelude.+ y0)
    where
      f x y
        | even y = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (unsafePred y  `quot` 2) x
      g x y z
        | even y = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (unsafePred y `quot` 2) (x <> z)
  {-# INLINE times1p #-}

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle1 :: Semigroup m => m -> m
cycle1 xs = xs' where xs' = xs <> xs'

instance Semigroup () where
  _ <> _ = ()
  sconcat _ = ()
  times1p _ _ = ()

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a
  times1p n f e = times1p n (f e)

instance Semigroup [a] where
  (<>) = (++)

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)

instance Semigroup (Either a b) where
  Left _ <> b = b
  a      <> _ = a

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a,b) <> (a',b') = (a<>a',b<>b')
  times1p n (a,b) = (times1p n a, times1p n b)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')
  times1p n (a,b,c) = (times1p n a, times1p n b, times1p n c)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a, b, c, d) where
  (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
  times1p n (a,b,c,d) = (times1p n a, times1p n b, times1p n c, times1p n d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (a, b, c, d, e) where
  (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')
  times1p n (a,b,c,d,e) = (times1p n a, times1p n b, times1p n c, times1p n d, times1p n e)

instance Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)
  times1p n (Dual a) = Dual (times1p n a)

instance Semigroup (Endo a) where
  Endo f <> Endo g = Endo (f . g)

instance Semigroup All where
  All a <> All b = All (a && b)
  times1p _ a = a

instance Semigroup Any where
  Any a <> Any b = Any (a || b)
  times1p _ a = a

instance Num a => Semigroup (Sum a) where
  Sum a <> Sum b = Sum (a + b)

instance Num a => Semigroup (Product a) where
  Product a <> Product b = Product (a * b)

instance Semigroup (Monoid.First a) where
  Monoid.First Nothing <> b = b
  a                    <> _ = a
  times1p _ a = a

instance Semigroup (Monoid.Last a) where
  a <> Monoid.Last Nothing = a
  _ <> b                   = b
  times1p _ a = a

instance Semigroup (NonEmpty a) where
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

newtype Min a = Min { getMin :: a } deriving
  ( Eq, Ord, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Ord a => Semigroup (Min a) where
  Min a <> Min b = Min (a `min` b)
  times1p _ a = a

instance (Ord a, Bounded a) => Monoid (Min a) where
  mempty = maxBound
  mappend = (<>)

newtype Max a = Max { getMax :: a } deriving
  ( Eq, Ord, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Ord a => Semigroup (Max a) where
  Max a <> Max b = Max (a `max` b)
  times1p _ a = a

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = minBound
  mappend = (<>)

-- | Use @'Option' ('First' a)@ -- to get the behavior of 'Data.Monoid.First'
newtype First a = First { getFirst :: a } deriving
  ( Eq, Ord, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data
  , Typeable
#endif
  )

instance Semigroup (First a) where
  a <> _ = a
  times1p _ a = a

-- | Use @'Option' ('Last' a)@ -- to get the behavior of 'Data.Monoid.Last'
newtype Last a = Last { getLast :: a } deriving
  ( Eq, Ord, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Semigroup (Last a) where
  _ <> b = b
  times1p _ a = a

-- (==)/XNOR on Bool forms a 'Semigroup', but has no good name


-- | Provide a Semigroup for an arbitrary Monoid.
newtype WrappedMonoid m = WrapMonoid
  { unwrapMonoid :: m } deriving
  ( Eq, Ord, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Monoid m => Semigroup (WrappedMonoid m) where
  WrapMonoid a <> WrapMonoid b = WrapMonoid (a `mappend` b)

instance Monoid m => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty
  WrapMonoid a `mappend` WrapMonoid b = WrapMonoid (a `mappend` b)


-- | Option is effectively 'Maybe' with a better instance of 'Monoid', built off of an underlying 'Semigroup'
-- instead of an underlying 'Monoid'. Ideally, this type would not exist at all and we would just fix the 'Monoid' intance of 'Maybe'
newtype Option a = Option
  { getOption :: Maybe a } deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Functor Option where
  fmap f (Option a) = Option (fmap f a)

instance Applicative Option where
  pure a = Option (Just a)
  Option a <*> Option b = Option (a <*> b)

instance Monad Option where
  return = pure

  Option (Just a) >>= k = k a
  _               >>= _ = Option Nothing

  Option Nothing  >>  _ = Option Nothing
  _               >>  b = b

instance Alternative Option where
  empty = Option Nothing
  Option Nothing <|> b = b
  a <|> _ = a

instance MonadPlus Option where
  mzero = empty
  mplus = (<|>)

instance MonadFix Option where
  mfix f = Option (mfix (getOption . f))

instance Foldable Option where
  foldMap f (Option (Just m)) = f m
  foldMap _ (Option Nothing)  = mempty

instance Traversable Option where
  traverse f (Option (Just a)) = Option . Just <$> f a
  traverse _ (Option Nothing)  = pure (Option Nothing)

option :: b -> (a -> b) -> Option a -> b
option n j (Option m) = maybe n j m

instance Semigroup a => Semigroup (Option a) where
  Option a <> Option b = Option (a <> b)

instance Semigroup a => Monoid (Option a) where
  mempty = empty
  Option a `mappend` Option b = Option (a <> b)

-- | This lets you use a difference list of a Semigroup as a Monoid.
diff :: Semigroup m => m -> Endo m
diff = Endo . (<>)

instance Semigroup (Seq a) where
  (<>) = (><)

instance Semigroup IntSet where
  (<>) = mappend
  times1p _ a = a

instance Ord a => Semigroup (Set a) where
  (<>) = mappend
  times1p _ a = a

instance Semigroup (IntMap v) where
  (<>) = mappend
  times1p _ a = a

instance Ord k => Semigroup (Map k v) where
  (<>) = mappend
  times1p _ a = a

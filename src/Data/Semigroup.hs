{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif
#ifdef LANGUAGE_DeriveGeneric
{-# LANGUAGE DeriveGeneric #-}
#endif
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup
-- Copyright   :  (C) 2011-2014 Edward Kmett
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
  , timesN
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

#ifndef BASE2
import Data.ByteString as Strict
import Data.ByteString.Lazy as Lazy
import qualified Data.Text as Strict
import qualified Data.Text.Lazy as Lazy
import Data.Hashable
import Data.HashMap.Lazy as Lazy
import Data.HashSet
#endif

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif
#ifdef LANGUAGE_DeriveGeneric
import GHC.Generics
#endif

infixr 6 <>

class Semigroup a where
  -- | An associative operation.
  --
  -- @
  -- (a '<>' b) '<>' c = a '<>' (b '<>' c)
  -- @
  --
  -- If @a@ is also a 'Monoid' we further require
  --
  -- @
  -- ('<>') = 'mappend'
  -- @
  (<>) :: a -> a -> a
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
  default (<>) :: Monoid a => a -> a -> a
  (<>) = mappend
#endif

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
  -- @
  -- 'times1p' n a = a '<>' a '<>' ... '<>' a  -- using '<>' n times
  -- @
  --
  -- The default definition uses peasant multiplication, exploiting associativity to only
  -- require /O(log n)/ uses of @\<\>@.
  --
  -- See also 'timesN'.

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
  times1p n x = rep n where
    rep 0 = x
    rep i = x ++ rep (i - 1)

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

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> y = y
  GT <> _ = GT

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

instance Semigroup a => Semigroup (Const a b) where
  Const a <> Const b = Const (a <> b)

#if MIN_VERSION_base(3,0,0)
instance Semigroup (Monoid.First a) where
  Monoid.First Nothing <> b = b
  a                    <> _ = a
  times1p _ a = a

instance Semigroup (Monoid.Last a) where
  a <> Monoid.Last Nothing = a
  _ <> b                   = b
  times1p _ a = a
#endif

instance Semigroup (NonEmpty a) where
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)

newtype Min a = Min { getMin :: a } deriving
  ( Eq, Ord, Enum, Bounded, Show, Read
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

instance Functor Min where
  fmap f (Min x) = Min (f x)

instance Foldable Min where
  foldMap f (Min a) = f a

instance Traversable Min where
  traverse f (Min a) = Min <$> f a

instance Applicative Min where
  pure = Min
  a <* _ = a
  _ *> a = a
  Min f <*> Min x = Min (f x)

instance Monad Min where
  return = Min
  _ >> a = a
  Min a >>= f = f a

instance MonadFix Min where
  mfix f = fix (f . getMin)

newtype Max a = Max { getMax :: a } deriving
  ( Eq, Ord, Enum, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#endif
  )

instance Ord a => Semigroup (Max a) where
  Max a <> Max b = Max (a `max` b)
  times1p _ a = a

instance (Ord a, Bounded a) => Monoid (Max a) where
  mempty = minBound
  mappend = (<>)

instance Functor Max where
  fmap f (Max x) = Max (f x)

instance Foldable Max where
  foldMap f (Max a) = f a

instance Traversable Max where
  traverse f (Max a) = Max <$> f a

instance Applicative Max where
  pure = Max
  a <* _ = a
  _ *> a = a
  Max f <*> Max x = Max (f x)

instance Monad Max where
  return = Max
  _ >> a = a
  Max a >>= f = f a

instance MonadFix Max where
  mfix f = fix (f . getMax)

-- | Use @'Option' ('First' a)@ to get the behavior of 'Data.Monoid.First' from @Data.Monoid@.
newtype First a = First { getFirst :: a } deriving
  ( Eq, Ord, Enum, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data
  , Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#endif
  )

instance Semigroup (First a) where
  a <> _ = a
  times1p _ a = a

instance Functor First where
  fmap f (First x) = First (f x)

instance Foldable First where
  foldMap f (First a) = f a

instance Traversable First where
  traverse f (First a) = First <$> f a

instance Applicative First where
  pure x = First x
  a <* _ = a
  _ *> a = a
  First f <*> First x = First (f x)

instance Monad First where
  return = First
  _ >> a = a
  First a >>= f = f a

instance MonadFix First where
  mfix f = fix (f . getFirst)

-- | Use @'Option' ('Last' a)@ to get the behavior of 'Data.Monoid.Last' from @Data.Monoid@
newtype Last a = Last { getLast :: a } deriving
  ( Eq, Ord, Enum, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#endif
  )

instance Semigroup (Last a) where
  _ <> b = b
  times1p _ a = a

instance Functor Last where
  fmap f (Last x) = Last (f x)
  a <$ _ = Last a

instance Foldable Last where
  foldMap f (Last a) = f a

instance Traversable Last where
  traverse f (Last a) = Last <$> f a

instance Applicative Last where
  pure = Last
  a <* _ = a
  _ *> a = a
  Last f <*> Last x = Last (f x)

instance Monad Last where
  return = Last
  _ >> a = a
  Last a >>= f = f a

instance MonadFix Last where
  mfix f = fix (f . getLast)

-- (==)/XNOR on Bool forms a 'Semigroup', but has no good name

#ifndef BASE2
instance Semigroup Strict.ByteString where
  (<>) = mappend

instance Semigroup Lazy.ByteString where
  (<>) = mappend

instance Semigroup Strict.Text where
  (<>) = mappend

instance Semigroup Lazy.Text where
  (<>) = mappend

instance (Hashable k, Eq k) => Semigroup (Lazy.HashMap k a) where
  (<>) = mappend

instance (Hashable a, Eq a) => Semigroup (HashSet a) where
  (<>) = mappend
  times1p _ a = a
#endif

-- | Provide a Semigroup for an arbitrary Monoid.
newtype WrappedMonoid m = WrapMonoid
  { unwrapMonoid :: m } deriving
  ( Eq, Ord, Enum, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#endif
  )

instance Monoid m => Semigroup (WrappedMonoid m) where
  WrapMonoid a <> WrapMonoid b = WrapMonoid (a `mappend` b)

instance Monoid m => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty
  WrapMonoid a `mappend` WrapMonoid b = WrapMonoid (a `mappend` b)

-- | Repeat a value @n@ times.
--
-- > timesN n a = a <> a <> ... <> a  -- using <> (n-1) times
--
-- Implemented using 'times1p'.
timesN :: (Whole n, Monoid a) => n -> a -> a
timesN n x | n == 0    = mempty
           | otherwise = unwrapMonoid . times1p (unsafePred n) . WrapMonoid $ x
{-# INLINE timesN #-}


-- | 'Option' is effectively 'Maybe' with a better instance of 'Monoid', built off of an underlying 'Semigroup'
-- instead of an underlying 'Monoid'.
--
-- Ideally, this type would not exist at all and we would just fix the 'Monoid' instance of 'Maybe'
newtype Option a = Option
  { getOption :: Maybe a } deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
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
  mzero = Option Nothing
  mplus = (<|>)

instance MonadFix Option where
  mfix f = Option (mfix (getOption . f))

instance Foldable Option where
  foldMap f (Option (Just m)) = f m
  foldMap _ (Option Nothing)  = mempty

instance Traversable Option where
  traverse f (Option (Just a)) = Option . Just <$> f a
  traverse _ (Option Nothing)  = pure (Option Nothing)

-- | Fold an 'Option' case-wise, just like 'maybe'.
option :: b -> (a -> b) -> Option a -> b
option n j (Option m) = maybe n j m

instance Semigroup a => Semigroup (Option a) where
  Option a <> Option b = Option (a <> b)

instance Semigroup a => Monoid (Option a) where
  mempty = Option Nothing
  Option a `mappend` Option b = Option (a <> b)

-- | This lets you use a difference list of a 'Semigroup' as a 'Monoid'.
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

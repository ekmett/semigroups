{-# LANGUAGE CPP #-}

#ifdef __GLASGOW_HASKELL__
#define LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

#if __GLASGOW_HASKELL__ >= 702
#define LANGUAGE_DeriveGeneric
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
#endif

#if __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds #-}
#endif

#if __GLASGOW_HASKELL__ >= 708
#define USE_COERCE
{-# LANGUAGE ScopedTypeVariables #-}
#endif

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Semigroup
-- Copyright   :  (C) 2011-2015 Edward Kmett
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
  , stimesMonoid
  , stimesIdempotent
  , stimesIdempotentMonoid
  , mtimesDefault
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
  -- * ArgMin, ArgMax
  , Arg(..)
  , ArgMin
  , ArgMax
  ) where

import Prelude hiding (foldr1)

#if MIN_VERSION_base(4,8,0)
import Data.Bifunctor
import Data.Void
#else
import Data.Monoid (Monoid(..))
import Data.Foldable
import Data.Traversable
#endif

import Data.Monoid (Dual(..),Endo(..),All(..),Any(..),Sum(..),Product(..))
#if MIN_VERSION_base(4,8,0)
import Data.Monoid (Alt(..))
#endif

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Control.Monad.ST as Strict
import qualified Data.Monoid as Monoid
import Data.List.NonEmpty
#if MIN_VERSION_base(4,6,0)
import Data.Ord (Down(..))
#else
import GHC.Exts (Down(..))
#endif
#if MIN_VERSION_base(4,4,0) && !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS) && !defined(ETA_VERSION)
import GHC.Event
#endif

#ifdef MIN_VERSION_deepseq
import Control.DeepSeq (NFData(..))
#endif

#ifdef MIN_VERSION_containers
import Data.Sequence (Seq, (><))
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.IntMap (IntMap)
#endif

#ifdef MIN_VERSION_binary
# if !(MIN_VERSION_binary(0,8,3))
import qualified Data.Binary.Builder as Builder
# endif
#endif

#ifdef MIN_VERSION_bytestring
import Data.ByteString as BS
import Data.ByteString.Lazy as BL

# if (MIN_VERSION_bytestring(0,10,2)) || defined(MIN_VERSION_bytestring_builder)
import qualified Data.ByteString.Builder as ByteString
# elif MIN_VERSION_bytestring(0,10,0)
import qualified Data.ByteString.Lazy.Builder as ByteString
# endif

# if (MIN_VERSION_bytestring(0,10,4)) || defined(MIN_VERSION_bytestring_builder)
import Data.ByteString.Short
# endif
#endif

#if (MIN_VERSION_base(4,8,0)) || defined(MIN_VERSION_transformers)
import Data.Functor.Identity
#endif

#if (MIN_VERSION_base(4,7,0)) || defined(MIN_VERSION_tagged)
import Data.Proxy
#endif

#ifdef MIN_VERSION_tagged
import Data.Tagged
#endif

#ifdef MIN_VERSION_text
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as Text
#endif

#ifdef MIN_VERSION_hashable
import Data.Hashable
#endif

#ifdef MIN_VERSION_unordered_containers
import Data.HashMap.Lazy as Lazy
import Data.HashSet
#endif

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

#ifdef LANGUAGE_DeriveGeneric
import GHC.Generics
#endif

#ifdef USE_COERCE
import Data.Coerce
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

  -- | Reduce a non-empty list with @\<\>@
  --
  -- The default definition should be sufficient, but this can be overridden for efficiency.
  --
  sconcat :: NonEmpty a -> a
  sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b

  -- | Repeat a value @n@ times.
  --
  -- Given that this works on a 'Semigroup' it is allowed to fail if you request 0 or fewer
  -- repetitions, and the default definition will do so.
  --
  -- By making this a member of the class, idempotent semigroups and monoids can upgrade this to execute in
  -- /O(1)/ by picking @stimes = stimesIdempotent@ or @stimes = stimesIdempotentMonoid@ respectively.
  --
  -- @since 0.17
  stimes :: Integral b => b -> a -> a
  stimes y0 x0
    | y0 <= 0   = error "stimes: positive multiplier expected"
    | otherwise = f x0 y0
    where
      f x y
        | even y = f (x <> x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x <> x) (y `quot` 2) x        -- See Note [Half of y - 1]
      g x y z
        | even y = g (x <> x) (y `quot` 2) z
        | y == 1 = x <> z
        | otherwise = g (x <> x) (y `quot` 2) (x <> z) -- See Note [Half of y - 1]
  {-# INLINE stimes #-}

{- Note [Half of y - 1]
   ~~~~~~~~~~~~~~~~~~~~~
   Since y is guaranteed to be odd and positive here,
   half of y - 1 can be computed as y `quot` 2, optimising subtraction away.
-}

-- | A generalization of 'Data.List.cycle' to an arbitrary 'Semigroup'.
-- May fail to terminate for some values in some semigroups.
cycle1 :: Semigroup m => m -> m
cycle1 xs = xs' where xs' = xs <> xs'

instance Semigroup () where
  _ <> _ = ()
  sconcat _ = ()
  stimes _ _ = ()

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a
  stimes n f e = stimes n (f e)

instance Semigroup [a] where
  (<>) = (++)
  stimes n x
    | n < 0 = error "stimes: [], negative multiplier"
    | otherwise = rep n
    where
      rep 0 = []
      rep i = x ++ rep (i - 1)

instance Semigroup a => Semigroup (Maybe a) where
  Nothing <> b       = b
  a       <> Nothing = a
  Just a  <> Just b  = Just (a <> b)
  stimes _ Nothing  = Nothing
  stimes n (Just a) = case compare n 0 of
    LT -> error "stimes: Maybe, negative multiplier"
    EQ -> Nothing
    GT -> Just (stimes n a)

instance Semigroup (Either a b) where
  Left _ <> b = b
  a      <> _ = a
  stimes = stimesIdempotent

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  (a,b) <> (a',b') = (a<>a',b<>b')
  stimes n (a,b) = (stimes n a, stimes n b)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')
  stimes n (a,b,c) = (stimes n a, stimes n b, stimes n c)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a, b, c, d) where
  (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')
  stimes n (a,b,c,d) = (stimes n a, stimes n b, stimes n c, stimes n d)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (a, b, c, d, e) where
  (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')
  stimes n (a,b,c,d,e) = (stimes n a, stimes n b, stimes n c, stimes n d, stimes n e)

instance Semigroup Ordering where
  LT <> _ = LT
  EQ <> y = y
  GT <> _ = GT
  stimes = stimesIdempotentMonoid

instance Semigroup a => Semigroup (Dual a) where
  Dual a <> Dual b = Dual (b <> a)
  stimes n (Dual a) = Dual (stimes n a)

instance Semigroup (Endo a) where
#ifdef USE_COERCE
  (<>) = coerce ((.) :: (a -> a) -> (a -> a) -> (a -> a))
#else
  Endo f <> Endo g = Endo (f . g)
#endif
  stimes = stimesMonoid

instance Semigroup All where
#ifdef USE_COERCE
  (<>) = coerce (&&)
#else
  All a <> All b = All (a && b)
#endif

  stimes = stimesIdempotentMonoid

instance Semigroup Any where
#ifdef USE_COERCE
  (<>) = coerce (||)
#else
  Any a <> Any b = Any (a || b)
#endif

  stimes = stimesIdempotentMonoid


instance Num a => Semigroup (Sum a) where
#ifdef USE_COERCE
  (<>) = coerce ((+) :: a -> a -> a)
#else
  Sum a <> Sum b = Sum (a + b)
#endif
  stimes n (Sum a) = Sum (fromIntegral n * a)

instance Num a => Semigroup (Product a) where
#ifdef USE_COERCE
  (<>) = coerce ((*) :: a -> a -> a)
#else
  Product a <> Product b = Product (a * b)
#endif
  stimes n (Product a) = Product (a ^ n)

instance Semigroup a => Semigroup (Down a) where
#ifdef USE_COERCE
  (<>) = coerce ((<>) :: a -> a -> a)
#else
  Down a <> Down b = Down (a <> b)
#endif
  stimes n (Down a) = Down (stimes n a)

-- | This is a valid definition of 'stimes' for a 'Monoid'.
--
-- Unlike the default definition of 'stimes', it is defined for 0
-- and so it should be preferred where possible.
stimesMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesMonoid n x0 = case compare n 0 of
  LT -> error "stimesMonoid: negative multiplier"
  EQ -> mempty
  GT -> f x0 n
    where
      f x y
        | even y = f (x `mappend` x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x `mappend` x) (y  `quot` 2) x              -- See Note [Half of y - 1]
      g x y z
        | even y = g (x `mappend` x) (y `quot` 2) z
        | y == 1 = x `mappend` z
        | otherwise = g (x `mappend` x) (y `quot` 2) (x `mappend` z) -- See Note [Half of y - 1]

-- | This is a valid definition of 'stimes' for an idempotent 'Monoid'.
--
-- When @mappend x x = x@, this definition should be preferred, because it
-- works in /O(1)/ rather than /O(log n)/
stimesIdempotentMonoid :: (Integral b, Monoid a) => b -> a -> a
stimesIdempotentMonoid n x = case compare n 0 of
  LT -> error "stimesIdempotentMonoid: negative multiplier"
  EQ -> mempty
  GT -> x
{-# INLINE stimesIdempotentMonoid #-}

-- | This is a valid definition of 'stimes' for an idempotent 'Semigroup'.
--
-- When @x <> x = x@, this definition should be preferred, because it
-- works in /O(1)/ rather than /O(log n)/.
stimesIdempotent :: Integral b => b -> a -> a
stimesIdempotent n x
  | n <= 0 = error "stimesIdempotent: positive multiplier expected"
  | otherwise = x
{-# INLINE stimesIdempotent #-}

instance Semigroup a => Semigroup (Const a b) where
#ifdef USE_COERCE
  (<>) = coerce ((<>) :: a -> a -> a)
#else
  Const a <> Const b = Const (a <> b)
#endif
  stimes n (Const a) = Const (stimes n a)

#if MIN_VERSION_base(3,0,0)
instance Semigroup (Monoid.First a) where
  Monoid.First Nothing <> b = b
  a                    <> _ = a
  stimes = stimesIdempotentMonoid

instance Semigroup (Monoid.Last a) where
  a <> Monoid.Last Nothing = a
  _ <> b                   = b
  stimes = stimesIdempotentMonoid
#endif

#if MIN_VERSION_base(4,8,0)
instance Alternative f => Semigroup (Alt f a) where
# ifdef USE_COERCE
  (<>) = coerce ((<|>) :: f a -> f a -> f a)
# else
  Alt a <> Alt b = Alt (a <|> b)
# endif
  stimes = stimesMonoid
#endif

#if MIN_VERSION_base(4,8,0)
instance Semigroup Void where
  a <> _ = a
  stimes = stimesIdempotent
#endif

instance Semigroup (NonEmpty a) where
  (a :| as) <> ~(b :| bs) = a :| (as ++ b : bs)


newtype Min a = Min { getMin :: a } deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

instance Bounded a => Bounded (Min a) where
  minBound = Min minBound
  maxBound = Min maxBound

instance Enum a => Enum (Min a) where
  succ (Min a) = Min (succ a)
  pred (Min a) = Min (pred a)
  toEnum = Min . toEnum
  fromEnum = fromEnum . getMin
  enumFrom (Min a) = Min <$> enumFrom a
  enumFromThen (Min a) (Min b) = Min <$> enumFromThen a b
  enumFromTo (Min a) (Min b) = Min <$> enumFromTo a b
  enumFromThenTo (Min a) (Min b) (Min c) = Min <$> enumFromThenTo a b c

#ifdef MIN_VERSION_hashable
instance Hashable a => Hashable (Min a) where
  hashWithSalt p (Min a) = hashWithSalt p a
#endif

instance Ord a => Semigroup (Min a) where
#ifdef USE_COERCE
  (<>) = coerce (min :: a -> a -> a)
#else
  Min a <> Min b = Min (a `min` b)
#endif
  stimes = stimesIdempotent

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

#ifdef MIN_VERSION_deepseq
instance NFData a => NFData (Min a) where
  rnf (Min a) = rnf a
#endif

instance Num a => Num (Min a) where
  (Min a) + (Min b) = Min (a + b)
  (Min a) * (Min b) = Min (a * b)
  (Min a) - (Min b) = Min (a - b)
  negate (Min a) = Min (negate a)
  abs    (Min a) = Min (abs a)
  signum (Min a) = Min (signum a)
  fromInteger    = Min . fromInteger

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
instance Generic1 Min where
  type Rep1 Min = D1 D1'Min (C1 C1'_0Min (S1 S1'_0_0Min Par1))
  from1 (Min x) = M1 (M1 (M1 (Par1 x)))
  to1 (M1 (M1 (M1 x))) = Min (unPar1 x)

instance Datatype D1'Min where
  datatypeName _ = "Min"
  moduleName   _ = "Data.Semigroup"

instance Constructor C1'_0Min where
  conName     _ = "Min"
  conIsRecord _ = True

instance Selector S1'_0_0Min where
  selName _ = "getMin"

data D1'Min
data C1'_0Min
data S1'_0_0Min
#endif

newtype Max a = Max { getMax :: a } deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

instance Bounded a => Bounded (Max a) where
  minBound = Max minBound
  maxBound = Max maxBound

instance Enum a => Enum (Max a) where
  succ (Max a) = Max (succ a)
  pred (Max a) = Max (pred a)
  toEnum = Max . toEnum
  fromEnum = fromEnum . getMax
  enumFrom (Max a) = Max <$> enumFrom a
  enumFromThen (Max a) (Max b) = Max <$> enumFromThen a b
  enumFromTo (Max a) (Max b) = Max <$> enumFromTo a b
  enumFromThenTo (Max a) (Max b) (Max c) = Max <$> enumFromThenTo a b c

#ifdef MIN_VERSION_hashable
instance Hashable a => Hashable (Max a) where
  hashWithSalt p (Max a) = hashWithSalt p a
#endif

instance Ord a => Semigroup (Max a) where
#ifdef USE_COERCE
  (<>) = coerce (max :: a -> a -> a)
#else
  Max a <> Max b = Max (a `max` b)
#endif
  stimes = stimesIdempotent

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

#ifdef MIN_VERSION_deepseq
instance NFData a => NFData (Max a) where
  rnf (Max a) = rnf a
#endif

instance Num a => Num (Max a) where
  (Max a) + (Max b) = Max (a + b)
  (Max a) * (Max b) = Max (a * b)
  (Max a) - (Max b) = Max (a - b)
  negate (Max a) = Max (negate a)
  abs    (Max a) = Max (abs a)
  signum (Max a) = Max (signum a)
  fromInteger    = Max . fromInteger

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
instance Generic1 Max where
  type Rep1 Max = D1 D1'Max (C1 C1'_0Max (S1 S1'_0_0Max Par1))
  from1 (Max x) = M1 (M1 (M1 (Par1 x)))
  to1 (M1 (M1 (M1 x))) = Max (unPar1 x)

instance Datatype D1'Max where
  datatypeName _ = "Max"
  moduleName   _ = "Data.Semigroup"

instance Constructor C1'_0Max where
  conName     _ = "Max"
  conIsRecord _ = True

instance Selector S1'_0_0Max where
  selName _ = "getMax"

data D1'Max
data C1'_0Max
data S1'_0_0Max
#endif

-- | 'Arg' isn't itself a 'Semigroup' in its own right, but it can be placed inside 'Min' and 'Max'
-- to compute an arg min or arg max.
data Arg a b = Arg a b deriving
  ( Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

type ArgMin a b = Min (Arg a b)
type ArgMax a b = Max (Arg a b)

instance Functor (Arg a) where
  fmap f (Arg x a) = Arg x (f a)

instance Foldable (Arg a) where
  foldMap f (Arg _ a) = f a

instance Traversable (Arg a) where
  traverse f (Arg x a) = Arg x <$> f a

instance Eq a => Eq (Arg a b) where
  Arg a _ == Arg b _ = a == b

instance Ord a => Ord (Arg a b) where
  Arg a _ `compare` Arg b _ = compare a b
  min x@(Arg a _) y@(Arg b _)
    | a <= b    = x
    | otherwise = y
  max x@(Arg a _) y@(Arg b _)
    | a >= b    = x
    | otherwise = y

#ifdef MIN_VERSION_deepseq
instance (NFData a, NFData b) => NFData (Arg a b) where
  rnf (Arg a b) = rnf a `seq` rnf b `seq` ()
#endif

#ifdef MIN_VERSION_hashable
#if MIN_VERSION_hashable(1,3,0)
-- | Instance like defined in @hashable-1.3@
instance Hashable a => Hashable (Arg a b) where
  hashWithSalt p (Arg a _b) = hashWithSalt p a
#else
-- | Instance like defined in @hashable-1.2@
instance (Hashable a, Hashable b) => Hashable (Arg a b) where
  hashWithSalt p (Arg a b) = hashWithSalt p a `hashWithSalt` b
#endif
#endif

#if MIN_VERSION_base(4,8,0)
instance Bifunctor Arg where
  bimap f g (Arg a b) = Arg (f a) (g b)
#endif

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
instance Generic1 (Arg a) where
  type Rep1 (Arg a)
    = D1 D1'Arg
        (C1 C1'_0Arg
             (S1 NoSelector (Rec0 a)
          :*: S1 NoSelector Par1))
  from1 (Arg a b) = M1 (M1 (M1 (K1 a) :*: M1 (Par1 b)))
  to1 (M1 (M1 (M1 a :*: M1 b))) = Arg (unK1 a) (unPar1 b)

instance Datatype D1'Arg where
  datatypeName _ = "Arg"
  moduleName   _ = "Data.Semigroup"

instance Constructor C1'_0Arg where
  conName _ = "Arg"

data D1'Arg
data C1'_0Arg
#endif

-- | Use @'Option' ('First' a)@ to get the behavior of 'Data.Monoid.First' from @Data.Monoid@.
newtype First a = First { getFirst :: a } deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data
  , Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

instance Bounded a => Bounded (First a) where
  minBound = First minBound
  maxBound = First maxBound

instance Enum a => Enum (First a) where
  succ (First a) = First (succ a)
  pred (First a) = First (pred a)
  toEnum = First . toEnum
  fromEnum = fromEnum . getFirst
  enumFrom (First a) = First <$> enumFrom a
  enumFromThen (First a) (First b) = First <$> enumFromThen a b
  enumFromTo (First a) (First b) = First <$> enumFromTo a b
  enumFromThenTo (First a) (First b) (First c) = First <$> enumFromThenTo a b c

#ifdef MIN_VERSION_hashable
instance Hashable a => Hashable (First a) where
  hashWithSalt p (First a) = hashWithSalt p a
#endif

instance Semigroup (First a) where
  a <> _ = a
  stimes = stimesIdempotent

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

#ifdef MIN_VERSION_deepseq
instance NFData a => NFData (First a) where
  rnf (First a) = rnf a
#endif

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
instance Generic1 First where
  type Rep1 First = D1 D1'First (C1 C1'_0First (S1 S1'_0_0First Par1))
  from1 (First x) = M1 (M1 (M1 (Par1 x)))
  to1 (M1 (M1 (M1 x))) = First (unPar1 x)

instance Datatype D1'First where
  datatypeName _ = "First"
  moduleName   _ = "Data.Semigroup"

instance Constructor C1'_0First where
  conName     _ = "First"
  conIsRecord _ = True

instance Selector S1'_0_0First where
  selName _ = "getFirst"

data D1'First
data C1'_0First
data S1'_0_0First
#endif

-- | Use @'Option' ('Last' a)@ to get the behavior of 'Data.Monoid.Last' from @Data.Monoid@
newtype Last a = Last { getLast :: a } deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

instance Bounded a => Bounded (Last a) where
  minBound = Last minBound
  maxBound = Last maxBound

instance Enum a => Enum (Last a) where
  succ (Last a) = Last (succ a)
  pred (Last a) = Last (pred a)
  toEnum = Last . toEnum
  fromEnum = fromEnum . getLast
  enumFrom (Last a) = Last <$> enumFrom a
  enumFromThen (Last a) (Last b) = Last <$> enumFromThen a b
  enumFromTo (Last a) (Last b) = Last <$> enumFromTo a b
  enumFromThenTo (Last a) (Last b) (Last c) = Last <$> enumFromThenTo a b c

#ifdef MIN_VERSION_hashable
instance Hashable a => Hashable (Last a) where
  hashWithSalt p (Last a) = hashWithSalt p a
#endif

instance Semigroup (Last a) where
  _ <> b = b
  stimes = stimesIdempotent

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

#ifdef MIN_VERSION_deepseq
instance NFData a => NFData (Last a) where
  rnf (Last a) = rnf a
#endif

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
instance Generic1 Last where
  type Rep1 Last = D1 D1'Last (C1 C1'_0Last (S1 S1'_0_0Last Par1))
  from1 (Last x) = M1 (M1 (M1 (Par1 x)))
  to1 (M1 (M1 (M1 x))) = Last (unPar1 x)

instance Datatype D1'Last where
  datatypeName _ = "Last"
  moduleName   _ = "Data.Semigroup"

instance Constructor C1'_0Last where
  conName     _ = "Last"
  conIsRecord _ = True

instance Selector S1'_0_0Last where
  selName _ = "getLast"

data D1'Last
data C1'_0Last
data S1'_0_0Last
#endif

-- (==)/XNOR on Bool forms a 'Semigroup', but has no good name

#ifdef MIN_VERSION_binary
# if !(MIN_VERSION_binary(0,8,3))
instance Semigroup Builder.Builder where
  (<>) = mappend
# endif
#endif

#ifdef MIN_VERSION_bytestring
instance Semigroup BS.ByteString where
  (<>) = mappend
  sconcat (b:|bs) = BS.concat (b:bs)

instance Semigroup BL.ByteString where
  (<>) = mappend
  sconcat (b:|bs) = BL.concat (b:bs)

# if (MIN_VERSION_bytestring(0,10,0)) || defined(MIN_VERSION_bytestring_builder)
instance Semigroup ByteString.Builder where
  (<>) = mappend
# endif

# if (MIN_VERSION_bytestring(0,10,4)) || defined(MIN_VERSION_bytestring_builder)
instance Semigroup ShortByteString where
  (<>) = mappend
# endif
#endif

#ifdef MIN_VERSION_text
instance Semigroup TS.Text where
  (<>) = mappend

instance Semigroup TL.Text where
  (<>) = mappend

instance Semigroup Text.Builder where
  (<>) = mappend
#endif

#ifdef MIN_VERSION_unordered_containers
instance (Hashable k, Eq k) => Semigroup (Lazy.HashMap k a) where
  (<>) = mappend
  stimes = stimesIdempotentMonoid

instance (Hashable a, Eq a) => Semigroup (HashSet a) where
  (<>) = mappend
  stimes = stimesIdempotentMonoid
#endif

-- | Provide a Semigroup for an arbitrary Monoid.
newtype WrappedMonoid m = WrapMonoid
  { unwrapMonoid :: m } deriving
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
#ifdef LANGUAGE_DeriveGeneric
  , Generic
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

#ifdef MIN_VERSION_hashable
instance Hashable a => Hashable (WrappedMonoid a) where
  hashWithSalt p (WrapMonoid a) = hashWithSalt p a
#endif

instance Monoid m => Semigroup (WrappedMonoid m) where
#ifdef USE_COERCE
  (<>) = coerce (mappend :: m -> m -> m)
#else
  WrapMonoid a <> WrapMonoid b = WrapMonoid (a `mappend` b)
#endif

instance Monoid m => Monoid (WrappedMonoid m) where
  mempty = WrapMonoid mempty
  mappend = (<>)

instance Bounded a => Bounded (WrappedMonoid a) where
  minBound = WrapMonoid minBound
  maxBound = WrapMonoid maxBound

instance Enum a => Enum (WrappedMonoid a) where
  succ (WrapMonoid a) = WrapMonoid (succ a)
  pred (WrapMonoid a) = WrapMonoid (pred a)
  toEnum = WrapMonoid . toEnum
  fromEnum = fromEnum . unwrapMonoid
  enumFrom (WrapMonoid a) = WrapMonoid <$> enumFrom a
  enumFromThen (WrapMonoid a) (WrapMonoid b) = WrapMonoid <$> enumFromThen a b
  enumFromTo (WrapMonoid a) (WrapMonoid b) = WrapMonoid <$> enumFromTo a b
  enumFromThenTo (WrapMonoid a) (WrapMonoid b) (WrapMonoid c) = WrapMonoid <$> enumFromThenTo a b c

#ifdef MIN_VERSION_deepseq
instance NFData m => NFData (WrappedMonoid m) where
  rnf (WrapMonoid a) = rnf a
#endif

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
instance Generic1 WrappedMonoid where
  type Rep1 WrappedMonoid = D1 D1'WrappedMonoid (C1 C1'_0WrappedMonoid (S1 S1'_0_0WrappedMonoid Par1))
  from1 (WrapMonoid x) = M1 (M1 (M1 (Par1 x)))
  to1 (M1 (M1 (M1 x))) = WrapMonoid (unPar1 x)

instance Datatype D1'WrappedMonoid where
  datatypeName _ = "WrappedMonoid"
  moduleName   _ = "Data.Semigroup"

instance Constructor C1'_0WrappedMonoid where
  conName     _ = "WrapMonoid"
  conIsRecord _ = True

instance Selector S1'_0_0WrappedMonoid where
  selName _ = "unwrapMonoid"

data D1'WrappedMonoid
data C1'_0WrappedMonoid
data S1'_0_0WrappedMonoid
#endif

-- | Repeat a value @n@ times.
--
-- > mtimesDefault n a = a <> a <> ... <> a  -- using <> (n-1) times
--
-- Implemented using 'stimes' and 'mempty'.
--
-- This is a suitable definition for an 'mtimes' member of 'Monoid'.
--
-- @since 0.17
mtimesDefault :: (Integral b, Monoid a) => b -> a -> a
mtimesDefault n x
  | n == 0    = mempty
  | otherwise = unwrapMonoid (stimes n (WrapMonoid x))

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
#if __GLASGOW_HASKELL__ >= 706
  , Generic1
#endif
#endif
  )

#ifdef MIN_VERSION_hashable
instance Hashable a => Hashable (Option a) where
  hashWithSalt p (Option a) = hashWithSalt p a
#endif

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

#ifdef MIN_VERSION_deepseq
instance NFData a => NFData (Option a) where
  rnf (Option a) = rnf a
#endif

-- | Fold an 'Option' case-wise, just like 'maybe'.
option :: b -> (a -> b) -> Option a -> b
option n j (Option m) = maybe n j m

instance Semigroup a => Semigroup (Option a) where
#ifdef USE_COERCE
  (<>) = coerce ((<>) :: Maybe a -> Maybe a -> Maybe a)
#else
  Option a <> Option b = Option (a <> b)
#endif
  stimes _ (Option Nothing) = Option Nothing
  stimes n (Option (Just a)) = case compare n 0 of
    LT -> error "stimes: Option, negative multiplier"
    EQ -> Option Nothing
    GT -> Option (Just (stimes n a))

instance Semigroup a => Monoid (Option a) where
  mempty = Option Nothing
  mappend = (<>)

#if __GLASGOW_HASKELL__ >= 702 && __GLASGOW_HASKELL__ < 706
instance Generic1 Option where
  type Rep1 Option = D1 D1'Option (C1 C1'_0Option (S1 S1'_0_0Option (Rec1 Maybe)))
  from1 (Option x) = M1 (M1 (M1 (Rec1 x)))
  to1 (M1 (M1 (M1 x))) = Option (unRec1 x)

instance Datatype D1'Option where
  datatypeName _ = "Option"
  moduleName   _ = "Data.Semigroup"

instance Constructor C1'_0Option where
  conName     _ = "Option"
  conIsRecord _ = True

instance Selector S1'_0_0Option where
  selName _ = "getOption"

data D1'Option
data C1'_0Option
data S1'_0_0Option
#endif

-- | This lets you use a difference list of a 'Semigroup' as a 'Monoid'.
diff :: Semigroup m => m -> Endo m
diff = Endo . (<>)

#ifdef MIN_VERSION_containers
instance Semigroup (Seq a) where
  (<>) = (><)

instance Semigroup IntSet where
  (<>) = mappend
  stimes = stimesIdempotentMonoid

instance Ord a => Semigroup (Set a) where
  (<>) = mappend
  stimes = stimesIdempotentMonoid

instance Semigroup (IntMap v) where
  (<>) = mappend
  stimes = stimesIdempotentMonoid

instance Ord k => Semigroup (Map k v) where
  (<>) = mappend
  stimes = stimesIdempotentMonoid
#endif

#if (MIN_VERSION_base(4,8,0)) || defined(MIN_VERSION_transformers)
instance Semigroup a => Semigroup (Identity a) where
# ifdef USE_COERCE
  (<>) = coerce ((<>) :: a -> a -> a)
# else
  Identity a <> Identity b = Identity (a <> b)
# endif
  stimes n (Identity a) = Identity (stimes n a)
#endif

#if (MIN_VERSION_base(4,7,0)) || defined(MIN_VERSION_tagged)
instance Semigroup (Proxy s) where
  _ <> _ = Proxy
  sconcat _ = Proxy
  stimes _ _ = Proxy
#endif

#ifdef MIN_VERSION_tagged
instance Semigroup a => Semigroup (Tagged s a) where
# ifdef USE_COERCE
  (<>) = coerce ((<>) :: a -> a -> a)
# else
  Tagged a <> Tagged b = Tagged (a <> b)
# endif
  stimes n (Tagged a) = Tagged (stimes n a)
#endif

instance Semigroup a => Semigroup (IO a) where
    (<>) = liftA2 (<>)

instance Semigroup a => Semigroup (Strict.ST s a) where
#if MIN_VERSION_base(4,4,0)
    (<>) = liftA2 (<>)
#else
    (<>) = liftM2 (<>) -- No Applicative instance for ST on GHC 7.0
#endif

#if !defined(mingw32_HOST_OS) && !defined(ghcjs_HOST_OS) && !defined(ETA_VERSION)
# if MIN_VERSION_base(4,4,0)
instance Semigroup Event where
    (<>) = mappend
    stimes = stimesMonoid
# endif

# if MIN_VERSION_base(4,8,1)
instance Semigroup Lifetime where
    (<>) = mappend
    stimes = stimesMonoid
# endif
#endif

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
  -- * Semigroups
  , Min(..)
  , Max(..)
  , First(..)
  , Last(..)
  , WrappedMonoid(..)
  -- * Monoids from Data.Monoid 
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
  ) where

import Prelude hiding (foldr1)
import Data.Monoid hiding (First(..), Last(..))
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Data.Monoid as Monoid
import Data.Foldable
import Data.Traversable
import Data.List.NonEmpty

import Data.Sequence (Seq, (><))
import Data.Set (Set)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.IntMap (IntMap)

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

infixl 6 <> 

class Semigroup a where
  (<>) :: a -> a -> a

  sconcat :: NonEmpty a -> a
  sconcat (a :| as) = go a as where
    go b (c:cs) = b <> go c cs
    go b []     = b

instance Semigroup () where
  _ <> _ = ()
  sconcat _ = ()

instance Semigroup b => Semigroup (a -> b) where
  f <> g = \a -> f a <> g a

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
  
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (a, b, c) where
  (a,b,c) <> (a',b',c') = (a<>a',b<>b',c<>c')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (a, b, c, d) where
  (a,b,c,d) <> (a',b',c',d') = (a<>a',b<>b',c<>c',d<>d')

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Semigroup e) => Semigroup (a, b, c, d, e) where
  (a,b,c,d,e) <> (a',b',c',d',e') = (a<>a',b<>b',c<>c',d<>d',e<>e')

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

instance Semigroup (Monoid.First a) where
  Monoid.First Nothing <> b = b
  a                    <> _ = a

instance Semigroup (Monoid.Last a) where
  a <> Monoid.Last Nothing = a
  _ <> b                   = b

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

-- | Use @'Option' ('Last' a)@ -- to get the behavior of 'Data.Monoid.Last'
newtype Last a = Last { getLast :: a } deriving 
  ( Eq, Ord, Bounded, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

instance Semigroup (Last a) where
  _ <> b = b

-- (==)/XNOR on Bool forms a 'Semigroup', but has no good name

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

instance Ord a => Semigroup (Set a) where
  (<>) = mappend

instance Semigroup (IntMap v) where
  (<>) = mappend

instance Ord k => Semigroup (Map k v) where
  (<>) = mappend

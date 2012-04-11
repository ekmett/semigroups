{-# LANGUAGE CPP #-}
#ifdef LANGUAGE_DeriveDataTypeable
{-# LANGUAGE DeriveDataTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.List.NonEmpty
-- Copyright   :  (C) 2011 Edward Kmett,
--                (C) 2010 Tony Morris, Oliver Taylor, Eelis van der Weegen
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- A NonEmpty list forms a monad as per list, but always contains at least
-- one element.
----------------------------------------------------------------------------

module Data.List.NonEmpty (
   -- * The type of streams
     NonEmpty(..)
   -- * non-empty stream transformations
   , map         -- :: (a -> b) -> NonEmpty a -> NonEmpty b
   , intersperse -- :: a -> NonEmpty a -> NonEmpty a
   , scanl       -- :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b
   , scanr       -- :: Foldable f => (a -> b -> b) -> b -> f a -> NonEmpty b
   , scanl1      -- :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
   , scanr1      -- :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
   --, transpose   -- :: NonEmpty (NonEmpty a) -> NonEmpty (NonEmpty a)
   -- * Basic functions
   , head        -- :: NonEmpty a -> a  
   , tail        -- :: NonEmpty a -> [a]
   , last        -- :: NonEmpty a -> a
   , init        -- :: NonEmpty a -> [a]
   , (<|), cons  -- :: a -> NonEmpty a -> NonEmpty a 
   , uncons      -- :: NonEmpty a -> (a, Maybe (NonEmpty a))
   , sort        -- :: NonEmpty a -> NonEmpty a
   , reverse     -- :: NonEmpty a -> NonEmpty a
   , inits       -- :: Foldable f => f a -> NonEmpty a
   , tails       -- :: Foldable f => f a -> NonEmpty a
   -- * Building streams
   , iterate     -- :: (a -> a) -> a -> NonEmpty a
   , repeat      -- :: a -> NonEmpty a 
   , cycle       -- :: NonEmpty a -> NonEmpty a
   , unfold      -- :: (a -> (b, Maybe a) -> a -> NonEmpty b
   , insert      -- :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
   -- * Extracting sublists
   , take        -- :: Int -> NonEmpty a -> [a]
   , drop        -- :: Int -> NonEmpty a -> [a]
   , splitAt     -- :: Int -> NonEmpty a -> ([a], [a])
   , takeWhile   -- :: Int -> NonEmpty a -> [a]
   , dropWhile   -- :: Int -> NonEmpty a -> [a]
   , span        -- :: Int -> NonEmpty a -> ([a],[a])
   , break       -- :: Int -> NonEmpty a -> ([a],[a])
   , filter      -- :: (a -> Bool) -> NonEmpty a -> [a]
   , partition   -- :: (a -> Bool) -> NonEmpty a -> ([a],[a])
   , group       -- :: Foldable f => Eq a => f a -> [NonEmpty a]
   , groupBy     -- :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
   , group1      -- :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
   , groupBy1    -- :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
   -- * Sublist predicates
   , isPrefixOf  -- :: Foldable f => f a -> NonEmpty a -> Bool
   -- * Indexing streams
   , (!!)        -- :: NonEmpty a -> Int -> a
   -- * Zipping and unzipping streams
   , zip         -- :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
   , zipWith     -- :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
   , unzip       -- :: NonEmpty (a, b) -> (NonEmpty a, NonEmpty b)
   -- * Functions on streams of characters
   , words       -- :: NonEmpty Char -> NonEmpty String
   , unwords     -- :: NonEmpty String -> NonEmpty Char
   , lines       -- :: NonEmpty Char -> NonEmpty String
   , unlines     -- :: NonEmpty String -> NonEmpty Char
   -- * Converting to and from a list
   , fromList    -- :: [a] -> NonEmpty a
   , toList      -- :: NonEmpty a -> [a]
   , nonEmpty    -- :: [a] -> Maybe (NonEmpty a)
   , xor         -- :: NonEmpty a -> Bool
   ) where


import Prelude hiding
  ( head, tail, map, reverse
  , scanl, scanl1, scanr, scanr1
  , iterate, take, drop, takeWhile
  , dropWhile, repeat, cycle, filter
  , (!!), zip, unzip, zipWith, words
  , unwords, lines, unlines, break, span
  , splitAt, foldr, foldl, last, init
  )

import Control.Applicative
-- import Control.Comonad
import Control.Monad
-- import Data.Functor.Alt
import Data.Foldable hiding (toList)
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import Data.Monoid (mappend)
import Data.Traversable
-- import Data.Semigroup hiding (Last)
-- import Data.Semigroup.Foldable
-- import Data.Semigroup.Traversable

#ifdef LANGUAGE_DeriveDataTypeable
import Data.Data
#endif

infixr 5 :|, <|

data NonEmpty a = a :| [a] deriving 
  ( Eq, Ord, Show, Read
#ifdef LANGUAGE_DeriveDataTypeable
  , Data, Typeable
#endif
  )

xor :: NonEmpty Bool -> Bool
xor (x :| xs)   = foldr xor' x xs
  where xor' True y  = not y
        xor' False y = y

unfold :: (a -> (b, Maybe a)) -> a -> NonEmpty b
unfold f a = case f a of
  (b, Nothing) -> b :| []
  (b, Just c)  -> b <| unfold f c

nonEmpty :: [a] -> Maybe (NonEmpty a)
nonEmpty []     = Nothing
nonEmpty (a:as) = Just (a :| as)
{-# INLINE nonEmpty #-}

uncons :: NonEmpty a -> (a, Maybe (NonEmpty a))
uncons ~(a :| as) = (a, nonEmpty as)
{-# INLINE uncons #-}

instance Functor NonEmpty where
  fmap f ~(a :| as) = f a :| fmap f as
#if MIN_VERSION_base(4,2,0)
  b <$ ~(_ :| as)   = b   :| (b <$ as)
#endif

{-
instance Extend NonEmpty where
  extend f w@ ~(_ :| aas) = f w :| case aas of
      []     -> []
      (a:as) -> toList (extend f (a :| as))

instance Comonad NonEmpty where
  extract ~(a :| _) = a
  
instance Apply NonEmpty where
  (<.>) = ap

instance Alt NonEmpty where
  (a :| as) <!> ~(b :| bs) = a :| (as ++ b : bs)
-}

instance Applicative NonEmpty where
  pure a = a :| []
  (<*>) = ap

instance Monad NonEmpty where
  return a = a :| []
  ~(a :| as) >>= f = b :| (bs ++ bs')
    where b :| bs = f a
          bs' = as >>= toList . f

instance Traversable NonEmpty where
  traverse f ~(a :| as) = (:|) <$> f a <*> traverse f as

{-
instance Traversable1 NonEmpty where
  traverse1 f (a :| []) = (:|[]) <$> f a
  traverse1 f (a :| (b: bs)) = (\a' (b':| bs') -> a' :| b': bs') <$> f a <.> traverse1 f (b :| bs)
-}

instance Foldable NonEmpty where
  foldr f z ~(a :| as) = f a (foldr f z as)
  foldl f z ~(a :| as) = foldl f (f z a) as 
  foldl1 f ~(a :| as) = foldl f a as
  foldMap f ~(a :| as) = f a `mappend` foldMap f as
  fold ~(m :| ms) = m `mappend` fold ms

{-
instance Foldable1 NonEmpty where
  foldMap1 f (a :| []) = f a
  foldMap1 f (a :| b : bs) = f a <> foldMap1 f (b :| bs)

instance Semigroup (NonEmpty a) where
  (<>) = (<!>)
-}

-- | Extract the first element of the stream
head :: NonEmpty a -> a
head ~(a :| _) = a
{-# INLINE head #-}

-- | Extract the possibly empty tail of the stream
tail :: NonEmpty a -> [a]
tail ~(_ :| as) = as
{-# INLINE tail #-}

-- | Extract the last element of the stream
last :: NonEmpty a -> a
last ~(a :| as) = List.last (a : as)
{-# INLINE last #-}

-- | Extract everything except the last element of the stream
init :: NonEmpty a -> [a]
init ~(a :| as) = List.init (a : as)
{-# INLINE init #-}

-- | cons onto a stream
(<|) :: a -> NonEmpty a -> NonEmpty a 
a <| ~(b :| bs) = a :| b : bs
{-# INLINE (<|) #-}

cons :: a -> NonEmpty a -> NonEmpty a
cons = (<|)
{-# INLINE cons #-}

-- | Sort a stream
sort :: Ord a => NonEmpty a -> NonEmpty a 
sort = lift List.sort
{-# INLINE sort #-}

-- | Converts an non-empty list to a stream.
fromList :: [a] -> NonEmpty a 
fromList (a:as) = a :| as
fromList [] = error "NonEmpty.fromList: empty list"
{-# INLINE fromList #-}

-- | Convert a stream to a list efficiently
toList :: NonEmpty a -> [a]
toList ~(a :| as) = a : as
{-# INLINE toList #-}

-- | Lift list operations to work on a 'NonEmpty' stream
lift :: Foldable f => ([a] -> [b]) -> f a -> NonEmpty b
lift f = fromList . f . Foldable.toList 
{-# INLINE lift #-}

-- | map a function over a 'NonEmpty' stream
map :: (a -> b) -> NonEmpty a -> NonEmpty b
map f ~(a :| as) = f a :| fmap f as 
{-# INLINE map #-}

-- | The 'inits' function takes a stream @xs@ and returns all the
-- finite prefixes of @xs@.
inits :: Foldable f => f a -> NonEmpty [a]
inits = fromList . List.inits . Foldable.toList
{-# INLINE inits #-}

-- | The 'tails' function takes a stream @xs@ and returns all the
-- suffixes of @xs@.
tails   :: Foldable f => f a -> NonEmpty [a]
tails = fromList . List.tails . Foldable.toList
{-# INLINE tails #-}

-- | 'insert' an item into a 'NonEmpty'
insert  :: (Foldable f, Ord a) => a -> f a -> NonEmpty a
insert a = fromList . List.insert a . Foldable.toList
{-# INLINE insert #-}

-- | 'scanl' is similar to 'foldl', but returns a stream of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == z :| [z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl   :: Foldable f => (b -> a -> b) -> b -> f a -> NonEmpty b
scanl f z = fromList . List.scanl f z . Foldable.toList
{-# INLINE scanl #-}

-- | 'scanr' is the right-to-left dual of 'scanl'.
-- Note that
--
-- > head (scanr f z xs) == foldr f z xs.
scanr   :: Foldable f => (a -> b -> b) -> b -> f a -> NonEmpty b
scanr f z = fromList . List.scanr f z . Foldable.toList
{-# INLINE scanr #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == x1 :| [x1 `f` x2, x1 `f` (x2 `f` x3), ...]
scanl1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanl1 f ~(a :| as) = fromList (List.scanl f a as)
{-# INLINE scanl1 #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (a -> a -> a) -> NonEmpty a -> NonEmpty a
scanr1 f ~(a :| as) = fromList (List.scanr1 f (a:as))
{-# INLINE scanr1 #-}

intersperse :: a -> NonEmpty a -> NonEmpty a
intersperse a ~(b :| bs) = b :| case bs of 
    [] -> []
    _ -> a : List.intersperse a bs
{-# INLINE intersperse #-}

-- | @'iterate' f x@ produces the infinite sequence
-- of repeated applications of @f@ to @x@.
--
-- > iterate f x = [x, f x, f (f x), ..]
iterate :: (a -> a) -> a -> NonEmpty a
iterate f a = a :| List.iterate f (f a)
{-# INLINE iterate #-}

-- | @'cycle' xs@ returns the infinite repetition of @xs@:
--
-- > cycle [1,2,3] = 1 :| [2,3,1,2,3,...]
cycle :: NonEmpty a -> NonEmpty a 
cycle = fromList . List.cycle . toList 
{-# INLINE cycle #-}

-- | 'reverse' a finite NonEmpty
reverse :: NonEmpty a -> NonEmpty a
reverse = lift List.reverse
{-# INLINE reverse #-}

-- | @'repeat' x@ returns a constant stream, where all elements are
-- equal to @x@.
repeat :: a -> NonEmpty a
repeat a = a :| List.repeat a
{-# INLINE repeat #-}

-- | @'take' n xs@ returns the first @n@ elements of @xs@.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
take :: Int -> NonEmpty a -> [a]
take n = List.take n . toList 
{-# INLINE take #-}

-- | @'drop' n xs@ drops the first @n@ elements off the front of
-- the sequence @xs@.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
drop :: Int -> NonEmpty a -> [a]
drop n = List.drop n . toList
{-# INLINE drop #-}

-- | @'splitAt' n xs@ returns a pair consisting of the prefix of @xs@ 
-- of length @n@ and the remaining stream immediately following this prefix.
--
-- /Beware/: passing a negative integer as the first argument will
-- cause an error.
splitAt :: Int -> NonEmpty a -> ([a],[a])
splitAt n = List.splitAt n . toList
{-# INLINE splitAt #-}

-- | @'takeWhile' p xs@ returns the longest prefix of the stream
-- @xs@ for which the predicate @p@ holds.
takeWhile :: (a -> Bool) -> NonEmpty a -> [a]
takeWhile p = List.takeWhile p . toList
{-# INLINE takeWhile #-}

-- | @'dropWhile' p xs@ returns the suffix remaining after
-- @'takeWhile' p xs@.
dropWhile :: (a -> Bool) -> NonEmpty a -> [a]
dropWhile p = List.dropWhile p . toList
{-# INLINE dropWhile #-}

-- | 'span' @p@ @xs@ returns the longest prefix of @xs@ that satisfies
-- @p@, together with the remainder of the stream.
span :: (a -> Bool) -> NonEmpty a -> ([a], [a])
span p = List.span p . toList
{-# INLINE span #-}

-- | The 'break' @p@ function is equivalent to 'span' @not . p@.
break :: (a -> Bool) -> NonEmpty a -> ([a], [a])
break p = span (not . p)
{-# INLINE break #-}

-- | 'filter' @p@ @xs@, removes any elements from @xs@ that do not satisfy @p@.
filter :: (a -> Bool) -> NonEmpty a -> [a]
filter p = List.filter p . toList
{-# INLINE filter #-}

-- | The 'partition' function takes a predicate @p@ and a stream
-- @xs@, and returns a pair of streams. The first stream corresponds
-- to the elements of @xs@ for which @p@ holds; the second stream
-- corresponds to the elements of @xs@ for which @p@ does not hold.
partition :: (a -> Bool) -> NonEmpty a -> ([a], [a])
partition p = List.partition p . toList 
{-# INLINE partition #-}

-- | The 'group' function takes a stream and returns a stream of
-- lists such that flattening the resulting stream is equal to the
-- argument.  Moreover, each sublist in the resulting stream
-- contains only equal elements.  For example,
--
-- > group $ cycle "Mississippi" = "M" : "i" : "ss" : "i" : "ss" : "i" : "pp" : "i" : "M" : "i" : ...
group :: (Foldable f, Eq a) => f a -> [NonEmpty a]
group = groupBy (==)
{-# INLINE group #-}

groupBy :: Foldable f => (a -> a -> Bool) -> f a -> [NonEmpty a]
groupBy eq0 = go eq0 . Foldable.toList
  where 
    go _  [] = []
    go eq (x : xs) = (x :| ys) : groupBy eq zs
      where (ys, zs) = List.span (eq x) xs
  
group1 :: Eq a => NonEmpty a -> NonEmpty (NonEmpty a)
group1 = groupBy1 (==)
{-# INLINE group1 #-}

groupBy1 :: (a -> a -> Bool) -> NonEmpty a -> NonEmpty (NonEmpty a)
groupBy1 eq (x :| xs) = (x :| ys) :| groupBy eq zs
  where (ys, zs) = List.span (eq x) xs
{-# INLINE groupBy1 #-}

-- | The 'isPrefix' function returns @True@ if the first argument is
-- a prefix of the second.
isPrefixOf :: Eq a => [a] -> NonEmpty a -> Bool
isPrefixOf [] _ = True
isPrefixOf (y:ys) (x :| xs) = (y == x) && List.isPrefixOf ys xs
{-# INLINE isPrefixOf #-}

-- | @xs !! n@ returns the element of the stream @xs@ at index
-- @n@. Note that the head of the stream has index 0.
--
-- /Beware/: passing a negative integer as the first argument will cause
-- an error.
(!!) :: NonEmpty a -> Int -> a
(!!) ~(x :| xs) n 
  | n == 0 = x
  | n > 0  = xs List.!! (n - 1)
  | otherwise = error "NonEmpty.!! negative argument"
{-# INLINE (!!) #-}

-- | The 'zip' function takes two streams and returns a list of
-- corresponding pairs.
zip :: NonEmpty a -> NonEmpty b -> NonEmpty (a,b)
zip ~(x :| xs) ~(y :| ys) = (x, y) :| List.zip xs ys
{-# INLINE zip #-}

-- | The 'zipWith' function generalizes 'zip'. Rather than tupling
-- the functions, the elements are combined using the function
-- passed as the first argument to 'zipWith'.
zipWith :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
zipWith f ~(x :| xs) ~(y :| ys) = f x y :| List.zipWith f xs ys
{-# INLINE zipWith #-}

-- | The 'unzip' function is the inverse of the 'zip' function.
unzip :: Functor f => f (a,b) -> (f a, f b)
unzip xs = (fst <$> xs, snd <$> xs)
{-# INLINE unzip #-}

-- | The 'words' function breaks a stream of characters into a
-- stream of words, which were delimited by white space.
words :: NonEmpty Char -> NonEmpty String
words = lift List.words
{-# INLINE words #-}

-- | The 'unwords' function is an inverse operation to 'words'. It
-- joins words with separating spaces.
unwords :: NonEmpty String -> NonEmpty Char
unwords = lift List.unwords
{-# INLINE unwords #-}

-- | The 'lines' function breaks a stream of characters into a list
-- of strings at newline characters. The resulting strings do not
-- contain newlines.
lines :: NonEmpty Char -> NonEmpty String
lines = lift List.lines
{-# INLINE lines #-}

-- | The 'unlines' function is an inverse operation to 'lines'. It
-- joins lines, after appending a terminating newline to each.
unlines :: NonEmpty String -> NonEmpty Char
unlines = lift List.unlines
{-# INLINE unlines #-}

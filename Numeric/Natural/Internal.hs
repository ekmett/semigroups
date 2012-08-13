{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.Natural.Internal
-- Copyright   :  (C) 2011 Edward Kmett,
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module exposes the potentially unsafe operations that are sometimes
-- needed for efficiency: The Natural data constructor and unsafePred.
--
----------------------------------------------------------------------------
module Numeric.Natural.Internal
  ( Natural(..)
  , Whole(..)
  ) where

import Data.Word
import Data.Bits
import Data.Ix

newtype Natural = Natural { runNatural :: Integer } deriving (Eq,Ord,Ix)

instance Show Natural where
  showsPrec d (Natural n) = showsPrec d n

instance Read Natural where
  readsPrec d = map (\(n, s) -> (Natural n, s)) . readsPrec d

instance Num Natural where
  Natural n + Natural m = Natural (n + m)
  Natural n * Natural m = Natural (n * m)
  Natural n - Natural m | result < 0 = error "Natural.(-): negative result"
                        | otherwise  = Natural result
	where result = n - m
  abs (Natural n) = Natural n
  signum (Natural n) = Natural (signum n)
  fromInteger n 
    | n >= 0 = Natural n
    | otherwise = error "Natural.fromInteger: negative"

instance Bits Natural where
  Natural n .&. Natural m = Natural (n .&. m)
  Natural n .|. Natural m = Natural (n .|. m)
  xor (Natural n) (Natural m) = Natural (xor n m)
  complement _ = error "Bits.complement: Natural complement undefined"
  shift (Natural n) = Natural . shift n
  rotate (Natural n) = Natural . rotate n
  bit = Natural . bit
  setBit (Natural n) = Natural . setBit n
  clearBit (Natural n) = Natural . clearBit n
  complementBit (Natural n) = Natural . complementBit n
  testBit = testBit . runNatural 
  bitSize = bitSize . runNatural
  isSigned _ = False
  shiftL (Natural n) = Natural . shiftL n
  shiftR (Natural n) = Natural . shiftR n
  rotateL (Natural n) = Natural . rotateL n
  rotateR (Natural n) = Natural . rotateR n
#if MIN_VERSION_base(4,6,0)
  popCount = popCountDefault
#endif

instance Real Natural where
  toRational (Natural a) = toRational a

instance Enum Natural where
  pred (Natural 0) = error "Natural.pred: 0"
  pred (Natural n) = Natural (pred n)
  succ (Natural n) = Natural (succ n)
  fromEnum (Natural n) = fromEnum n
  toEnum n | n < 0     = error "Natural.toEnum: negative"
           | otherwise = Natural (toEnum n)

instance Integral Natural where
  quot (Natural a) (Natural b) = Natural (quot a b)
  rem (Natural a) (Natural b) = Natural (rem a b)
  div (Natural a) (Natural b) = Natural (div a b)
  mod (Natural a) (Natural b) = Natural (mod a b)
  divMod (Natural a) (Natural b) = (Natural q, Natural r) where (q,r) = divMod a b
  quotRem (Natural a) (Natural b) = (Natural q, Natural r) where (q,r) = quotRem a b
  toInteger = runNatural

-- | A refinement of Integral to represent types that do not contain negative numbers.
class Integral n => Whole n where
  toNatural :: n -> Natural
  unsafePred :: n -> n

instance Whole Word where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word8 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word16 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word32 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Word64 where
  toNatural = Natural . toInteger
  unsafePred n = n - 1

instance Whole Natural where
  toNatural = id
  unsafePred (Natural n) = Natural (n - 1)

{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Semigroup.Act where

import Data.Semigroup

-- | Represents an action of semigroup @g@ to set @a@.
--
-- Laws: @'Endo' . 'act'@ must be a homomorphism of semigroups.
class Semigroup g => SemigroupAct g a where
    act :: g -> (a -> a)

-- | Represents an action of monoid @g@ to set @a@.
--
-- Laws: @'Endo' . 'act'@ must be a homomorphism of monoids.
class (Monoid g, SemigroupAct g a) => MonoidAct g a where

-- | A wrapper for constructing a monoid action from 'Option'.
newtype OptionSet g a = OptionSet { getOptionSet :: a }

instance (SemigroupAct g a, Semigroup g)
  => SemigroupAct (Option g) (OptionSet g a) where
    act (Option Nothing)  x = x
    act (Option (Just g)) (OptionSet x) = OptionSet $ (act g) x
instance (SemigroupAct g a, Monoid g)
  => MonoidAct (Option g) (OptionSet g a) where


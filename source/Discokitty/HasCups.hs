{-|
Module: HasCups
Description: A cup operation for a meaning type.
License: GPL-3

Required operations for a meaning type to have some form of formal
cups. These can be used for Lambek reductions on the meaning of the
sentence.
|-}

module Discokitty.HasCups where

class HasCups m where
  -- | Given an integer, performs that number of cups between the two
  -- meaning spaces.  In a monoidal category, this should be the composition
  -- of the tensor product of two states with a certain number of cups.
  cup :: Int -> m -> m -> m

  -- | Neutral element for the cup operation. This could be given by the
  -- identity morphism (state).
  cunit :: m

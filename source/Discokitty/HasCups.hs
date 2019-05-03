{-|
Module: HasCups
Description: A cup operation for a meaning type.
License: GPL-3

Required operations for a meaning type to have some form of formal
cups. These can be used in Lambek reductions later.
|-}

module Discokitty.HasCups where

class HasCups m where
  -- | Given an integer, performs that number of cups between the two
  -- meaning spaces.
  cup :: Int -> m -> m -> m

  -- | Neutral element for the cup operation.
  cunit :: m


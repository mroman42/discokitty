module Discokitty.Dimension where

class Dim a where
  -- | In the meaning space, states can have a given number of output
  -- wires. This function takes an state and should output the number
  -- of output wires it has.
  dim :: a -> Int

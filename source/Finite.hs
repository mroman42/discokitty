module Finite where

class Finite u where
  -- | We can list the universe of elements
  universe :: [u]

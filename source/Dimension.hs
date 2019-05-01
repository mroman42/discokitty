module Dimension where

class Dim a where
  -- | In the meaning space, objects have dimensions.
  dim :: a -> Int

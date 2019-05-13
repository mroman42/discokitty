{-|
Module: Vectorspaces
Description: Sparse representation of vectors over an arbitrary semiring.
License: GPL-3

An implementation of the cups of the category of matrices over an
arbitary semiring. In this module we call "vector space" to what would
be more generally a module over a semiring.  The representation is done
using sparse vectors that do not include the elements of the basis whose
element is zero.
|-}

module Discokitty.Models.Vectorspaces
  ( Vectorspace (..)
  , sparse
  , fromList
  , fromMap
  , toMap
  , Semiring (..)
  )
where

import           Data.List
import qualified Data.Map             as Map
import           Data.Maybe
import           Discokitty.Dimension
import           Discokitty.HasCups

class (Eq m, Ord m) => Semiring m where
  plus :: m -> m -> m
  mult :: m -> m -> m
  zero :: m
  unit :: m

-- | A vector is given internally by a map representing the
-- coefficients of each basis element.
data Vectorspace u m = Vector (Map.Map [u] m)

-- | Shows the coefficients of the vector.
instance (Show m, Show u) => Show (Vectorspace u m) where
  show = show . toMap

-- | Creates a sparse vector from a list of basis elements multiplied
-- by scalars.
sparse :: (Ord u, Eq u, Semiring m) => [([u], m)] -> Vectorspace u m
sparse = fromList

-- | Creates a sparse vector from a map assigning a scalar to each
-- base element.
fromMap :: Map.Map [u] m -> Vectorspace u m
fromMap = Vector

-- | Outputs a map assigning to each base element its coefficient.
toMap :: Vectorspace u m -> Map.Map [u] m
toMap (Vector v) = v

toList :: Vectorspace u m -> [([u], m)]
toList = Map.toList . toMap

fromList :: (Ord u, Eq u, Semiring m) => [([u], m)] -> Vectorspace u m
fromList = fromMap . removeZerosM . Map.fromList . nubPlus
 where
  nubPlus :: (Ord u, Eq u, Semiring m) => [([u], m)] -> [([u], m)]
  nubPlus = fmap addTogether . (groupBy (\x y -> fst x == fst y))
  addTogether :: (Ord u, Eq u, Semiring m) => [([u], m)] -> ([u], m)
  addTogether []             = undefined
  addTogether l@((u, _) : _) = (u, foldr plus zero (fmap snd l))

-- | Auxiliary function that removes zeroes from the sparse
-- representation as a map.
removeZerosM :: (Semiring m) => Map.Map [u] m -> Map.Map [u] m
removeZerosM = Map.filter (/= zero)

-- | Auxiliary function that removes zeroes from the sparse
-- representation.
removeZeros :: (Eq u, Semiring m) => Vectorspace u m -> Vectorspace u m
removeZeros = fromMap . removeZerosM . toMap

-- | Auxiliary function that adds together coefficients for the same
-- basis element.
removePlus :: (Ord u, Eq u, Semiring m) => Vectorspace u m -> Vectorspace u m
removePlus = fromList . toList

-- | Auxiliary function that converts a formal sum into a vector both
-- adding up coefficients for the same basis elements and removing
-- zeroes.
normalize :: (Ord u, Eq u, Semiring m) => Vectorspace u m -> Vectorspace u m
normalize = removePlus . removeZeros

instance Dim (Vectorspace u m) where
  dim = dimVec

dimVec :: Vectorspace u m -> Int
dimVec = dimList . Map.toList . toMap
 where
  dimList []      = 0
  dimList (l : _) = length (fst l)

-- | The cup opreation for vectors. Implements the scalar product.
vecCup
  :: (Ord u, Eq u, Semiring m)
  => Int
  -> Vectorspace u m
  -> Vectorspace u m
  -> Vectorspace u m
vecCup n r s = normalize . fromList . catMaybes . fmap (agrees n) $ do
  (a, x) <- toList r
  (b, y) <- toList s
  return ((a, b), mult x y)

-- | The unit for the cup is just the identity state for vector
-- spaces.
vecUnit :: (Ord u, Eq u, Semiring m) => Vectorspace u m
vecUnit = fromList [([], unit)]

-- | Checks if two vectors have a shared basis element with a non zero
-- coefficient.  This is an auxiliary function for the scalar product.
agrees :: (Eq u, Semiring m) => Int -> (([u], [u]), m) -> Maybe ([u], m)
agrees n ((x, y), m) = if take n (reverse x) == take n y
  then Just $ (reverse (drop n (reverse x)) ++ drop n y, m)
  else Nothing

instance (Ord u, Eq u, Semiring m) => HasCups (Vectorspace u m) where
  cup   = vecCup
  cunit = vecUnit

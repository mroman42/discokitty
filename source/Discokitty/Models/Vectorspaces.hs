module Discokitty.Models.Vectorspaces where

-- Implementation of cups in the category of matrices over a semiring.

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

data Vectorspace u m = Vector (Map.Map [u] m)

instance (Show m, Show u) => Show (Vectorspace u m) where
  show = show . toMap

fromMap :: Map.Map [u] m -> Vectorspace u m
fromMap = Vector

toMap :: Vectorspace u m -> Map.Map [u] m
toMap (Vector v) = v

toList :: Vectorspace u m -> [([u] , m)]
toList = Map.toList . toMap

fromList :: (Ord u, Eq u, Semiring m) => [([u] , m)] -> Vectorspace u m
fromList = fromMap . removeZerosM . Map.fromList . nubPlus
  where
    nubPlus :: (Ord u, Eq u, Semiring m) => [([u] , m)] -> [([u] , m)]
    nubPlus = fmap addTogether . (groupBy (\ x y -> fst x == fst y))
    addTogether :: (Ord u, Eq u, Semiring m) => [([u] , m)] -> ([u] , m)
    addTogether []              = undefined
    addTogether l@((u , _) : _) = (u , foldr plus zero (fmap snd l))

removeZerosM :: (Semiring m) => Map.Map [u] m -> Map.Map [u]  m
removeZerosM = Map.filter (/= zero)

removeZeros :: (Eq u, Semiring m) => Vectorspace u m -> Vectorspace u m
removeZeros = fromMap . removeZerosM . toMap

removePlus :: (Ord u, Eq u, Semiring m) => Vectorspace u m -> Vectorspace u m
removePlus = fromList . toList

normalize :: (Ord u, Eq u, Semiring m) => Vectorspace u m -> Vectorspace u m
normalize = removePlus . removeZeros

instance Dim (Vectorspace u m) where
  dim = dimVec

dimVec :: Vectorspace u m -> Int
dimVec = dimList . Map.toList . toMap
  where
    dimList []      = 0
    dimList (l : _) = length (fst l)

vecCup :: (Ord u, Eq u, Semiring m) => Int -> Vectorspace u m -> Vectorspace u m -> Vectorspace u m
vecCup n r s = normalize . fromList . catMaybes . fmap (agrees n) $ do
  (a , x) <- toList r
  (b , y) <- toList s
  return ((a,b) , mult x y)

vecUnit :: (Ord u, Eq u, Semiring m) => Vectorspace u m
vecUnit = fromList [([], unit)]

agrees :: (Eq u, Semiring m) => Int -> (([u] , [u]) , m) -> Maybe ([u] , m)
agrees n ((x , y) , m) =
  if take n (reverse x) == take n y
    then Just $ (reverse (drop n (reverse x)) ++ drop n y , m)
    else Nothing

instance (Ord u, Eq u, Semiring m) => HasCups (Vectorspace u m) where
  cup   = vecCup
  cunit = vecUnit

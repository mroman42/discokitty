module Vectorspaces where

-- Implementation of cups in the category of matrices over a semiring.

import qualified Data.Map   as Map
import           Data.Maybe
import           Dimension
import           HasCups
import           Universe

import           Data.List

class (Eq m, Ord m) => Semiring m where
  plus :: m -> m -> m
  mult :: m -> m -> m
  zero :: m
  unit :: m

data Vectorspace m = Vector (Map.Map UniverseN m)

instance (Show m) => Show (Vectorspace m) where
  show = show . toMap

fromMap :: Map.Map UniverseN m -> Vectorspace m
fromMap = Vector

toMap :: Vectorspace m -> Map.Map UniverseN m
toMap (Vector v) = v

toList :: Vectorspace m -> [(UniverseN , m)]
toList = Map.toList . toMap

fromList :: (Semiring m) => [(UniverseN , m)] -> Vectorspace m
fromList = fromMap . removeZerosM . Map.fromList . nubPlus
  where
    nubPlus :: (Semiring m) => [(UniverseN , m)] -> [(UniverseN , m)]
    nubPlus = fmap addTogether . (groupBy (\ x y -> fst x == fst y))
    addTogether :: (Semiring m) => [(UniverseN , m)] -> (UniverseN , m)
    addTogether []            = undefined
    addTogether l@((u , x):t) = (u , foldr plus zero (fmap snd l))

removeZerosM :: (Semiring m) => Map.Map UniverseN m -> Map.Map UniverseN  m
removeZerosM = Map.filter (/= zero)

removeZeros :: (Semiring m) => Vectorspace m -> Vectorspace m
removeZeros = fromMap . removeZerosM . toMap

removePlus :: (Semiring m) => Vectorspace m -> Vectorspace m
removePlus = fromList . toList

normalize :: (Semiring m) => Vectorspace m -> Vectorspace m
normalize = removePlus . removeZeros

instance Dim (Vectorspace m) where
  dim = dimVec

dimVec :: Vectorspace m -> Int
dimVec = dimList . Map.toList . toMap
  where
    dimList []      = 0
    dimList (l : _) = length (fst l)

vecCup :: (Semiring m) => Int -> Vectorspace m -> Vectorspace m -> Vectorspace m
vecCup n r s = normalize . fromList . catMaybes . fmap (agrees n) $ do
  (a , x) <- toList r
  (b , y) <- toList s
  return ((a,b) , mult x y)

vecUnit :: (Semiring m) => Vectorspace m
vecUnit = fromList [([], unit)]

agrees :: (Semiring m) => Int -> ((UniverseN , UniverseN) , m) -> Maybe (UniverseN , m)
agrees n ((x , y) , m) =
  if take n (reverse x) == take n y
    then Just $ (reverse (drop n (reverse x)) ++ drop n y , m)
    else Nothing

instance (Semiring m) => HasCups (Vectorspace m) where
  cup   = vecCup
  cunit = vecUnit

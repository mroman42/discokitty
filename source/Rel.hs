{-# OPTIONS -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TupleSections             #-}

-- An implementation of the cups and objects of the category of
-- relations.

{-|
Module: Rel
Description: Cups and objects of the category of relations.
License: GPL-3
|-}

module Rel
  ( Rel
  , fromList
  , toList
  , idn
  , relCup
  , agrees
  )
where

import           Data.Maybe
import qualified Data.Set   as S
import           Dimension
import           HasCups
import           Universe

-- A relation hom(1,a) is given by a subset of the universe with
-- elements in a.
data Rel = Rel (S.Set UniverseN)

fromList :: [UniverseN] -> Rel
fromList = Rel . S.fromList

toList :: Rel -> [UniverseN]
toList (Rel u) = S.toList u

instance Show Rel where
  show = show . toList

instance Dim Rel where
  dim = dimRel

dimRel :: Rel -> Int
dimRel = dimList . toList
  where
    dimList []      = 0
    dimList (l : _) = length l

idn :: Int -> Rel
idn n = fromList $ do
  u <- universe
  return $ replicate n u

relCup :: Int -> Rel -> Rel -> Rel
relCup n r s = fromList $ catMaybes $ fmap (agrees n) $ do
  x <- toList r
  y <- toList s
  return (x,y)

relCunit :: Rel
relCunit = fromList [[]]

agrees :: Int -> (UniverseN , UniverseN) -> Maybe UniverseN
agrees n (x , y) =
  if take n (reverse x) == take n y
    then Just $ reverse (drop n (reverse x)) ++ drop n y
    else Nothing

instance HasCups Rel where
  cup = relCup
  cunit = relCunit

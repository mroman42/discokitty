{-# OPTIONS -Wall #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE TupleSections             #-}

{-|
Module: Rel
Description: Cups and objects of the category of relations.
License: GPL-3
|-}

module Discokitty.Models.Rel
  ( Rel
  , relation
  , fromList
  , toList
  , relCup
  , agrees
  )
where

import           Data.Maybe
import qualified Data.Set             as S
import           Discokitty.Dimension
import           Discokitty.HasCups
import           Discokitty.Words

-- | A relation hom(1,a) is given by a subset of the universe with
-- elements in a. We model this using the Data.Set library.
data Rel u = Rel (S.Set [u])

relation :: (Ord u) => [[u]] -> Rel u
relation = Rel . S.fromList

fromList :: (Ord u) => [[u]] -> Rel u
fromList = relation

toList :: Rel u -> [[u]]
toList (Rel u) = S.toList u

instance (Show u) => Show (Rel u) where
  show = show . toList

instance Dim (Rel u) where
  dim = dimRel

dimRel :: Rel u -> Int
dimRel = dimList . toList
 where
  dimList []      = 0
  dimList (l : _) = length l

relCup :: (Ord u) => Int -> Rel u -> Rel u -> Rel u
relCup n r s = relation $ catMaybes $ fmap (agrees n) $ do
  x <- toList r
  y <- toList s
  return (x, y)

relCunit :: (Ord u) => Rel u
relCunit = relation [[]]

agrees :: (Eq u) => Int -> ([u], [u]) -> Maybe [u]
agrees n (x, y) = if take n (reverse x) == take n y
  then Just $ reverse (drop n (reverse x)) ++ drop n y
  else Nothing

instance (Ord u) => HasCups (Rel u) where
  cup = relCup
  cunit = relCunit

instance Dim (Words (Rel u)) where
  dim = dim . meaning

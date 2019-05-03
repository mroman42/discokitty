{-# LANGUAGE FlexibleInstances #-}

{-|
Module: Words
Description: Data for a word in the Discocat framework.
License: GPL-3
|-}

module Discokitty.Words
  ( Words (..)
  , sentence
  , concatenate
  , emptyWord
  , (@@@)
  )
where

import           Data.Maybe
import           Discokitty.Dimension
import           Discokitty.HasCups
import           Discokitty.Lambek

-- | A word is given by a meaning and a grammatical type.  The Words
-- type is parameterized over the meaning type.
data Words m = Words
  { meaning :: m
  , grammar :: Lambek
  }

instance Show m => Show (Words m) where
  show w = show (meaning w) ++ " of grammar type " ++ show (grammar w)


size :: Words m -> Int
size w = length (grammar w)

maybeCon :: (HasCups m) => Int -> Words m -> Words m -> Maybe (Words m)
maybeCon n u v =
  if agreeOn n (grammar u) (grammar v)
    then Just $ Words
      { meaning = (cup n (meaning u) (meaning v))
      , grammar = reverse (drop n (reverse $ grammar u)) ++ drop n (grammar v)
      }
    else Nothing


tryConcatenate :: (HasCups m) => Int -> Words m -> Words m -> [Words m]
tryConcatenate n a b = catMaybes $ [maybeCon m a b | m <- [0..n]]

concatenate :: (HasCups m) => Words m -> Words m -> [Words m]
concatenate a b = tryConcatenate (min (size a) (size b)) a b


(@@@) :: [Words m] -> Lambek -> [Words m]
ws @@@ l = filter (\ x -> grammar x == l) ws

(...) :: (HasCups m) => Words m -> [Words m] -> [Words m]
w ... xs = concat $ do
  x <- xs
  return (concatenate w x)

emptyWord :: (HasCups m) => Words m
emptyWord = Words cunit []

sentence :: (HasCups m) => [Words m] -> [Words m]
sentence = foldr (...) [emptyWord]
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
  , (@@@@)
  )
where

import           Data.Maybe
import           Discokitty.HasCups
import           Discokitty.Lambek

-- | A word is given by a meaning and a grammatical type.  The Words
-- type is parameterized over the meaning type.
data Words m = Words
  { meaning :: m
  , grammar :: Lambek
  , text    :: String
  }

instance Show m => Show (Words m) where
  show w = show (meaning w) ++ " of grammar type " ++ show (grammar w)

-- | Size of the word, i.e. number of output wires or atoms in the
-- Lambek grammatical type.
size :: Words m -> Int
size w = length (grammar w)

-- | Tries to concatenate two words a given number of times. Fails if
-- the grammar types do not coincide.
maybeCon :: (HasCups m) => Int -> Words m -> Words m -> Maybe (Words m)
maybeCon n u v = if agreeOn n (grammar u) (grammar v)
  then Just Words
    { meaning = cup n (meaning u) (meaning v)
    , grammar = reverse (drop n (reverse $ grammar u)) ++ drop n (grammar v)
    , text    = text u ++ " " ++ text v
    }
  else Nothing

-- | Tries all possible reductions of two words up to a given number
-- of cups.  It outputs all the ones that are successful, that is, the
-- ones making the grammatical types match.
tryConcatenate :: (HasCups m) => Int -> Words m -> Words m -> [Words m]
tryConcatenate n a b = catMaybes [ maybeCon m a b | m <- [0 .. n] ]

concatenate :: (HasCups m) => Words m -> Words m -> [Words m]
concatenate a b = tryConcatenate (min (size a) (size b)) a b

-- | Filters a list of words by grammatical type.
(@@@) :: [Words m] -> Lambek -> [Words m]
ws @@@ l = filter (\x -> grammar x == l) ws

(@@@@) :: [Words m] -> Lambek -> Words m
ws @@@@ l = head $ (ws @@@ l)

(...) :: (HasCups m) => Words m -> [Words m] -> [Words m]
w ... xs = concat $ concatenate w <$> xs

-- | Empty word. Unit for concatenation of words.
emptyWord :: (HasCups m) => Words m
emptyWord = Words cunit [] ""

-- | Concatenates a list of words outputting all possible grammatical
-- reductions.
sentence :: (HasCups m) => [Words m] -> [Words m]
sentence = foldr (...) [emptyWord]

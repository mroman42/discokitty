{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- A finite universe for the play.
module Discokitty.Examples.LesJustesUniverse
  ( Universe (..)
  , UniverseN
  , universe
  , dim
  )
  where

import           Discokitty

data Universe
  = Universe

  -- Nouns
  | Yanek
  | Dora
  | Boris
  | Duke
  | Stepan
  | Nephew
  | Skouratov

  -- Adjectives
  | Poet
  | Revolutionary
  | Terrorist
  | Saviour
  | Innocent
  | Tsarist
  | Alive

  -- Things
  | Life
  | Poetry
  | Chemistry
  | Propaganda

  | Bomb

  -- Sentence meanings
  | IsTrue
  | IsFalse
  | IsRighteous
  | IsWrong
  | IsPlot

  deriving (Eq, Show, Bounded, Enum, Ord)

universe :: [Universe]
universe = [minBound .. maxBound]

type UniverseN = [Universe]

instance Dim UniverseN where
  dim = length


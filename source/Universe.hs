{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- A finite universe for the play.
module Universe
  ( Universe (..)
  , universe
  , UniverseN
  , dim
  )
  where

import           Dimension

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

-- Enumerate all possible values.
universe :: [Universe]
universe = [minBound .. maxBound]

type UniverseN = [Universe]

instance Dim UniverseN where
  dim = length

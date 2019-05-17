{-|
Module: Les justes, viterbi (2)
Description: Self-contained Les Justes example.
|-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Discokitty.Examples.LesJustesViterbi2 where

import           Discokitty
import           Discokitty.Models.Diagrams
import           Discokitty.Models.Vectorspaces

-- Universe
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

  -- Sentence meanings
  | IsTrue

  deriving (Eq, Show, Bounded, Enum, Ord)

universe :: [Universe]
universe = [minBound .. maxBound]


-- Viterbi semiring
newtype Viterbi = Viterbi Double deriving (Eq, Show, Num, Ord)
instance Semiring Viterbi where
  plus = max
  mult = (*)
  unit = 1
  zero = 0

v :: Double -> Viterbi
v = Viterbi

type Term = Words (Vectorspace Universe Viterbi)


likes :: Term
likes = Words
  { meaning = sparse
    [ ([Yanek, IsTrue, Dora]       , v 0.9)
    , ([Dora, IsTrue, Yanek]       , v 0.8)
    , ([Stepan, IsTrue, Dora]      , v 0.6)
    , ([Dora, IsTrue, Poetry]      , v 0.8)
    , ([Dora, IsTrue, Chemistry]   , v 1)
    , ([Yanek, IsTrue, Poetry]     , v 1)
    , ([Yanek, IsTrue, Life]       , v 0.9)
    , ([Dora, IsTrue, Life]        , v 0.8)
    , ([Stepan, IsTrue, Propaganda], v 0.9)
    , ([Stepan, IsTrue, Life]      , v 0.1)
    , ([Boris, IsTrue, Life]       , v 0.3)
    , ([Boris, IsTrue, Propaganda] , v 0.6)
    ]
  , grammar = [L N, S, R N]
  , text    = "likes"
  }

combats :: Term
combats = Words
  { meaning = sparse
    [ ([Yanek, IsTrue, Duke]      , v 1)
    , ([Yanek, IsTrue, Skouratov] , v 0.7)
    , ([Dora, IsTrue, Duke]       , v 0.8)
    , ([Dora, IsTrue, Skouratov]  , v 0.4)
    , ([Stepan, IsTrue, Duke]     , v 1)
    , ([Stepan, IsTrue, Skouratov], v 0.9)
    , ([Stepan, IsTrue, Nephew]   , v 0.7)
    , ([Boris, IsTrue, Duke]      , v 0.9)
    , ([Boris, IsTrue, Nephew]    , v 0.1)
    , ([Skouratov, IsTrue, Yanek] , v 0.9)
    , ([Skouratov, IsTrue, Stepan], v 1)
    ]
  , grammar = [L N, S, R N]
  , text    = "combats"
  }

is :: Term
is = Words
  { meaning = sparse
    [ ([Yanek, IsTrue, Revolutionary] , v 0.9)
    , ([Yanek, IsTrue, Poet]          , v 1)
    , ([Dora, IsTrue, Poet]           , v 0.5)
    , ([Dora, IsTrue, Revolutionary]  , v 0.7)
    , ([Boris, IsTrue, Revolutionary] , v 0.7)
    , ([Stepan, IsTrue, Terrorist]    , v 0.95)
    , ([Yanek, IsTrue, Terrorist]     , v 0.25)
    , ([Boris, IsTrue, Terrorist]     , v 0.25)
    , ([Stepan, IsTrue, Revolutionary], v 0.8)
    , ([Duke, IsTrue, Tsarist]        , v 1)
    , ([Skouratov, IsTrue, Tsarist]   , v 0.9)
    , ([Nephew, IsTrue, Tsarist]      , v 0.3)
    , ([Nephew, IsTrue, Innocent]     , v 1)
    , ([Yanek, IsTrue, Innocent]      , v 0.5)
    ]
  , grammar = [L N, S, R N]
  , text    = "is"
  }

people :: Term
people = Words
  { meaning = sparse
    [ ([Yanek]    , v 1)
    , ([Dora]     , v 1)
    , ([Stepan]   , v 1)
    , ([Duke]     , v 1)
    , ([Nephew]   , v 1)
    , ([Skouratov], v 1)
    , ([Boris]    , v 1)
    ]
  , grammar = [N]
  , text    = "people"
  }

who :: Term
who = Words
  { meaning = sparse [([a, a, b, a], v 1) | a <- universe, b <- universe]
  , grammar = [L N, N, R S, N]
  , text = "who"
  }

tsarist :: Term
tsarist = Words
  { meaning = sparse [([Tsarist] , v 1)]
  , grammar = [N]
  , text = "tsarist"
  }

tsarists :: Term
tsarists = sentence [people , who , is , tsarist] @@@@ [N]

exampleViterbi :: Term
exampleViterbi = sentence [people , who , combats , tsarists] @@@@ [N]

exampleDiagramViterbi :: String
exampleDiagramViterbi = tikzDiagrams [people , who , combats , people , who , is , tsarist]

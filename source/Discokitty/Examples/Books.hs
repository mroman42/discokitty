module Discokitty.Examples.Books where

import           Discokitty
import           Discokitty.Models.Diagrams
import           Discokitty.Models.Vectorspaces

-- People who love books that Mary wrote.

-- Universe of our example
data Universe
  = Mary
  | Percy
  | John

  | Frankenstein
  | Valperga
  | Vampyre

  | IsTrue
  deriving (Eq, Show, Bounded, Enum, Ord)

universe :: [Universe]
universe = [minBound .. maxBound]

-- We take the semiring structure of the real numbers.
instance Semiring Double where
  plus = (+)
  mult = (*)
  unit = 1
  zero = 0


type Term = Words (Vectorspace Universe Double)

mary :: Term
mary = Words
  { meaning = sparse [([Mary], 1)]
  , grammar = [N]
  , text = "Mary"
  }

people :: Term
people = Words
  { meaning = sparse [([Mary], 1), ([John], 1), ([Percy], 1)]
  , grammar = [N]
  , text    = "People"
  }

love :: Term
love = Words
  { meaning = sparse
     [ ([ Mary , IsTrue , Percy ] , 0.9)
     , ([ Percy , IsTrue , Mary ] , 0.9)
     , ([ Percy , IsTrue , Frankenstein ] , 0.9)
     , ([ Percy , IsTrue , Vampyre ] , 0.5)
     , ([ John , IsTrue , Frankenstein ] , 0.6)
     , ([ John , IsTrue , Valperga ] , 0.7)
     , ([ Mary , IsTrue , Frankenstein ] , 0.8)
     , ([ John , IsTrue , Vampyre ] , 1)
     ]
  , grammar = [L N , S , R N]
  , text = "loves"
  }

books :: Term
books = Words
  { meaning = sparse
      [ ([Frankenstein], 0.95)
      , ([Valperga], 0.8)
      , ([Vampyre], 0.6)
      ]
  , grammar = [N]
  , text    = "books"
  }

wrote :: Term
wrote = Words
  { meaning = sparse
      [ ([Mary, IsTrue, Frankenstein], 1)
      , ([Mary, IsTrue, Valperga], 1)
      , ([John, IsTrue, Vampyre], 1)
      ]
  , grammar = [L N, S, N]
  , text    = "wrote"
  }

that :: Term
that = Words
  { meaning = sparse [([a, a, a, b], 1) | a <- universe, b <- universe]
  , grammar = [L N, N, R N, R S]
  , text = "that"
  }

who :: Term
who = Words
  { meaning = sparse [([a, a, b, a], 1) | a <- universe, b <- universe]
  , grammar = [L N, N, R S, N]
  , text = "that"
  }

exampleBooks :: [Term]
exampleBooks = sentence [people , who , love , books , that , mary , wrote] @@@ [N]

exampleBooksDiagram :: String
exampleBooksDiagram = tikzDiagrams [people , who , love , books , that , mary , wrote]

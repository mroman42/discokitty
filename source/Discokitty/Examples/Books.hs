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
  deriving (Eq, Show, Bounded, Enum, Ord)

-- We take the semiring structure of the real numbers.
instance Semiring Double where
  plus = (+)
  mult = (*)
  unit = 1
  zero = 0


type Term = Words (Vectorspace Universe Double)

mary :: Term
mary = Words
  { meaning = sparse [ ([Mary] , 1) ]
  , grammar = [N]
  , text    = "Mary"
  }

people :: Term
people = Words
  { meaning = sparse
      [ ([Mary] , 1)
      , ([John] , 1)
      , ([Percy] , 1)
      ]
  , grammar = [N]
  , text = "People"
  }

books :: Term
books = Words
  { meaning = sparse
      [ ([Frankenstein] , 0.9)
      , ([Valperga] , 1)
      , ([Vampyre] , 0.7)
      ]
  , grammar = [N]
  , text = "books"
  }

wrote :: Term
wrote = Words
  { meaning = sparse
      [ ([Mary , Frankenstein] , 1)
      , ([Mary , Valperga] , 1)
      , ([John , Vampyre] , 1)
      ]
  , grammar = [L N , S , N]
  , text = "wrote"
  }

that :: Term
that = Words
  { meaning = sparse
      [ ([] , 1)
      ]
  }

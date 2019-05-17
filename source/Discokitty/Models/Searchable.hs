module Discokitty.Searchable where

class Searchable s where
  search :: s a -> (a -> Bool) -> Maybe a

data Relation s u = Relation (s [u])


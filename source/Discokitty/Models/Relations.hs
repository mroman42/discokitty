module Discokitty.Models.Relations where



-- class Searchable where
-- existence :: Int -> (m [u] -> Bool) -> Bool

-- class Quantale q where
--   and :: q -> q -> q
--   union :: [q] -> q

-- type Relation m u q = Relation (m [u] -> q)

-- relationCup :: (Ord u)
--   => Int -> Relation m u q -> Relation m u q -> Relation m u q
-- relationCup n r s =
--   a <- dropLast n r
--   b <- dropLast n s
--   or u
--   such that  (r a `and` s b)

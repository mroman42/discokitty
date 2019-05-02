module Examples.AliceAndBob where

import           Lambek
import           Models.Rel
import           Words

-- We first declare an universe with all the possible basis words,
-- both nouns and sentences.  The rest of the types are parameterized
-- by this universe.
data Universe = Alice | Bob | IsTrue | IsFalse deriving (Eq, Ord, Show)

-- We choose to use the category of relations for this example, and we
-- declare a term to be a word in the category of relations for our
-- given universe.
type Term = Words.Words (Models.Rel.Rel Universe)

-- We give meaning to some terms.  Relations are described as subsets using
-- "fromList", and the Lambek grammatical type must be written at the end.
alice, bob, loves :: Term
alice = Words (fromList [ [ Alice ] ]) [N]
bob   = Words (fromList [ [ Bob ] ]) [N]
loves = Words (fromList [ [ Alice , IsTrue , Bob ] ]) [ L N , S , R N ]

-- In our example sentence, we evaluate "Alice loves Bob".
example :: [Term]
example = sentence [alice , loves , bob] @@@ [S]

-- This produces the following output:
--   > [[IsTrue]] of grammar type [S]

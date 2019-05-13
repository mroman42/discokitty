{-|
Module: Multiwords
Description: Multiwords represent non deterministic words.
License: GPL-3

A multiword is given by some finite set of meaning/gramamr pairs. They
generalize words and can be used to represent reductions where multiple
parsings are possible but also words with multiple acceptations.

This is experimental and it is not part of the standard DisCoCat
framework.
|-}
module Discokitty.Multiwords where

import           Data.List
import           Data.Semigroup
import           Discokitty.HasCups
import           Discokitty.Lambek
import           Discokitty.Words

-- | The probability is given by real numbers.
type Probability = Double

-- | A multiword is given by a list of different words with different
-- probabilities. Note that these words do not need to have the same
-- grammar types.
newtype Multiword m = Multiword [(Words m , Probability)]

-- | Shows a multiword as a list of acceptations.
instance (Show m) => Show (Multiword m) where
  show =
    intercalate "\n" .
    fmap (\ (w, p) -> show w ++ " with p=" ++ show p) .
    toList

toList :: Multiword m -> [(Words m, Probability)]
toList (Multiword a) = a

fromList :: [(Words m, Probability)] -> Multiword m
fromList = Multiword

singleton :: Words m -> Multiword m
singleton w = fromList [(w, 1.0)]

-- | Concatenates the meaning of multiple multiwords using the formal
-- cups on the meaning category.
multiconcat :: (HasCups m) => Multiword m -> Multiword m -> Multiword m
multiconcat x y = fromList $ do
  (w, p) <- toList x
  (v, q) <- toList y
  let concats = concatenate w v
  let newprob = (p * q) / fromIntegral (length concats)
  zip concats (repeat newprob)

infixr 4 `multiconcat`

-- | The empty word for a formal cups multiword.
multiempty :: (HasCups m) => Multiword m
multiempty = fromList [(emptyWord, 1)]

instance (HasCups m) => Semigroup (Multiword m) where
  (<>) = multiconcat

instance (HasCups m) => Monoid (Multiword m) where
  mempty = multiempty
  mappend = multiconcat

-- | Concatenates a whole sentence of multiwords into a single one.
sentence :: (HasCups m) => [Multiword m] -> Multiword m
sentence = mconcat

-- | Filters the acceptations of the multiword that match the given
-- Lambek type.
(@@) :: Multiword m -> Lambek -> Multiword m
ws @@ l = fromList $ fmap (\(x, p) -> (x, p / totalprob)) newlist
 where
  totalprob = sum $ fmap snd newlist
  newlist   = filter (\(x, _) -> grammar x == l) (toList ws)


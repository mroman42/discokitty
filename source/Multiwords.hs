module Multiwords where

import           Data.List
import           Data.Semigroup
import           Dimension
import           HasCups
import           Lambek
import           Rel            hiding (fromList, toList)
import           Words

type Probability = Double

-- A multiword is given by a list of different words with different
-- probabilities. Note that these words do not need to have the same
-- grammar types.
data Multiword m = Multiword [(Words m , Probability)]

instance (Show m) => Show (Multiword m) where
  show =
    concat .
    intersperse "\n" .
    fmap (\ (w, p) -> show w ++ " with p=" ++ show p) .
    toList

toList :: Multiword m -> [(Words m , Probability)]
toList (Multiword a) = a

fromList :: [(Words m , Probability)] -> Multiword m
fromList = Multiword

singleton :: Words m -> Multiword m
singleton w = fromList [(w,1.0)]

multiconcat :: (HasCups m) => Multiword m -> Multiword m -> Multiword m
multiconcat x y = fromList $ do
  (w , p) <- toList x
  (v , q) <- toList y
  let concats = concatenate w v
  let newprob = (p * q) / fromIntegral (length concats)
  zip concats (repeat newprob)

infixr 4 `multiconcat`

multiempty :: (HasCups m) => Multiword m
multiempty = fromList [( emptyWord , 1 )]

instance (HasCups m) => Semigroup (Multiword m) where
  (<>) = multiconcat



instance (HasCups m) => Monoid (Multiword m) where
  mempty = multiempty
  mappend = multiconcat

sentence :: (HasCups m) => [Multiword m] -> Multiword m
sentence = mconcat

(@@) :: Multiword m -> Lambek -> Multiword m
ws @@ l = fromList $ fmap (\ (x,p) -> (x , p / totalprob)) newlist
  where
    totalprob = sum $ fmap snd newlist
    newlist = filter (\ (x , _) -> grammar x == l) (toList ws)


{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MainVit where

import qualified Data.Map     as Map
import           Lambek
import           MainVec
import qualified Multiwords   as M
import           Vectorspaces
import           Words

-- Viterbi semiring
newtype Viterbi = Viterbi Double deriving (Eq, Show, Num, Ord)
instance Semiring Viterbi where
  plus = max
  mult = (*)
  unit = 1
  zero = 0

-- Reals -> Viterbi translation
v :: M.Multiword (Vectorspace Double) -> M.Multiword (Vectorspace Viterbi)
v = M.fromList . fmap (\ (x , p) -> (v' x , p) ) . M.toList
  where
    v' :: Words (Vectorspace Double) -> Words (Vectorspace Viterbi)
    v' w = w { meaning = v'' (meaning w) }

    v'' :: Vectorspace Double -> Vectorspace Viterbi
    v'' = fromMap . Map.map Viterbi . toMap

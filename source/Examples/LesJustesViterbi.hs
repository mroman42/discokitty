{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Examples.LesJustesViterbi where

import qualified Data.Map            as Map
import           Lambek              ()
import           Models.Vectorspaces
import qualified Multiwords          as M
import           Words

-- Viterbi semiring
newtype Viterbi = Viterbi Double deriving (Eq, Show, Num, Ord)
instance Semiring Viterbi where
  plus = max
  mult = (*)
  unit = 1
  zero = 0

-- Reals -> Viterbi translation
v :: M.Multiword (Vectorspace u Double) -> M.Multiword (Vectorspace u Viterbi)
v = M.fromList . fmap (\ (x , p) -> (v' x , p) ) . M.toList
  where
    v' :: Words (Vectorspace u Double) -> Words (Vectorspace u Viterbi)
    v' w = w { meaning = v'' (meaning w) }

    v'' :: Vectorspace u Double -> Vectorspace u Viterbi
    v'' = fromMap . Map.map Viterbi . toMap

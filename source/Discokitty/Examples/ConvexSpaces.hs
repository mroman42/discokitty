module Discokitty.Examples.ConvexSpaces where

import           Control.Monad
import           Discokitty
import           Discokitty.Models.Rel

data Color = Color
  { red   :: Int
  , green :: Int
  , blue  :: Int
  }
  deriving (Eq, Show, Ord)

anyColor :: [Color]
anyColor = do
  r <- [0..2]
  g <- [0..2]
  b <- [0..2]
  return $ Color r g b

data Taste = Taste
  { sweet  :: Int
  , sour   :: Int
  , bitter :: Int
  }
  deriving (Eq, Show, Ord)

anyTaste :: [Taste]
anyTaste = do
  sw <- [0..2]
  so <- [0..2]
  bi <- [0..2]
  return $ Taste sw so bi

data Reaction = Reaction
  { positive   :: Bool
  , surprising :: Bool
  }
  deriving (Eq, Show, Ord)

data Universe = Noun Color Taste | Sentence Reaction deriving (Ord, Eq, Show)

type Term = Words (Rel Universe)

banana :: Term
banana = Words
  { meaning = relation bananaMeaning
  , grammar = [N]
  , text = "banana"
  }
  where
    bananaMeaning :: [[Universe]]
    bananaMeaning = do
      c <- bananaColor
      t <- bananaTaste
      return $ [ Noun c t ]

    bananaTaste :: [Taste]
    bananaTaste = do
      sw <- [1..2]
      so <- [0..1]
      bi <- [0..1]
      guard (so + bi <= 1)
      return $ Taste sw so bi

    bananaColor :: [Color]
    bananaColor = do
      r <- [1..2]
      b <- [0..0]
      g <- [0..2]
      guard (9 * r  <= 10 * g)
      guard (15 * r >= 10 * g)
      return $ Color r g b

beer :: Term
beer = Words
  { meaning = relation beerMeaning
  , grammar = [N]
  , text = "beer"
  }
  where
    beerMeaning :: [[Universe]]
    beerMeaning = do
      -- Close to yellow
      c <- anyColor
      t <- anyTaste
      guard (green c <= red c)
      guard (red c <= 2 * green c)
      guard (blue c <= 1)

      -- It is bitter and can be a bit sweet and sour
      guard (sweet t <= 1)
      guard (sour t <= 1)
      guard (bitter t >= 2)

      return [Noun c t]

greenAdj :: Term
greenAdj = Words
  { meaning = relation greenMeaning
  , grammar = [N , R N]
  , text = "green"
  }
  where
    greenMeaning :: [[Universe]]
    greenMeaning = do
      c <- anyColor
      guard (green c >= 2)
      guard (blue c <= 1)
      guard (red c <= 1)
      t <- anyTaste
      return $ [ Noun c t ]

yellowAdj :: Term
yellowAdj = Words
  { meaning = relation yellowMeaning
  , grammar = [N , R N]
  , text = "yellow"
  }
  where
    yellowMeaning :: [[Universe]]
    yellowMeaning = do
      c <- anyColor
      guard (green c >= 1)
      guard (blue c <= 0)
      guard (red c >= 1)
      t <- anyTaste
      return $ [ Noun c t ]

bitterMeaning :: [[Universe]]
bitterMeaning = do
  t <- anyTaste
  guard (bitter t >= 2)
  guard (sweet t <= 1)
  guard (sour t <= 1)
  c <- anyColor
  return [Noun c t]

bitterAdj :: Term
bitterAdj = Words
  { meaning = relation [b ++ b | b <- bitterMeaning]
  , grammar = [N , R N]
  , text = "bitter"
  }

bitterNoun :: Term
bitterNoun = Words
  { meaning = relation bitterMeaning
  , grammar = [N]
  , text = "bitter"
  }


sweetNoun :: Term
sweetNoun = Words
  { meaning = relation sweetMeaning
  , grammar = [N]
  , text = "sweet"
  }

sweetMeaning :: [[Universe]]
sweetMeaning = do
  t <- anyTaste
  guard (sweet t >= 2)
  guard (bitter t <= 0)
  guard (sour t <= 0)
  c <- anyColor
  return [Noun c t]

sweetAdj :: Term
sweetAdj = Words
  { meaning = relation $ do
      s <- sweetMeaning
      return (s ++ s)
  , grammar = [N , L N]
  , text = "sweet"
  }

greenBanana :: Term
greenBanana = sentence [greenAdj , banana] @@@@ [N]

yellowBanana :: Term
yellowBanana = sentence [yellowAdj , banana] @@@@ [N]

tastes :: Term
tastes = Words
  { meaning = relation
      $ tasteMeaning1 ++ tasteMeaning2 ++ tasteMeaning3 ++ tasteMeaning4
  , grammar = [ L N , S , R N ]
  , text = "tastes"
  }
  where
    -- Green bananas taste bitter, this is not surprising nor positive.
    tasteMeaning1 :: [[Universe]]
    tasteMeaning1 = do
      g <- toList $ meaning greenBanana
      t <- toList $ meaning bitterNoun
      return $ g ++ [Sentence (Reaction False False)] ++ t

    -- When green bananas taste sweet, that is surprising and positive.
    tasteMeaning2 :: [[Universe]]
    tasteMeaning2 = do
      g <- toList $ meaning greenBanana
      t <- toList $ meaning sweetNoun
      return $ g ++ [Sentence (Reaction True True)] ++ t

    -- Yellow bananas taste sweet, this is positive but unsurprising.
    tasteMeaning3 :: [[Universe]]
    tasteMeaning3 = do
      g <- toList $ meaning yellowBanana
      t <- toList $ meaning sweetNoun
      return $ g ++ [Sentence (Reaction True False)] ++ t

    -- Beer tasting sweet is surprising and negative.
    tasteMeaning4 :: [[Universe]]
    tasteMeaning4 = do
      g <- toList $ meaning beer
      t <- toList $ meaning sweetNoun
      return $ g ++ [Sentence (Reaction False True)] ++ t

exampleConvexbool :: Bool
exampleConvexbool =
  [Sentence (Reaction False True)] `elem`
    (toList (meaning (sentence [beer , tastes , sweetNoun] @@@@ [S])))

exampleConvex :: Term
exampleConvex = sentence [beer , tastes , sweetNoun] @@@@ [S]


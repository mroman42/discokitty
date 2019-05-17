{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Discokitty.Examples.LesJustesVector where

import           Data.Semigroup
import           Discokitty
import           Discokitty.Examples.LesJustesUniverse
import           Discokitty.Models.Vectorspaces
import qualified Discokitty.Multiwords                 as M



-- The real numbers have the obvious semiring structure.
instance Semiring Double where
  plus = (+)
  mult = (*)
  unit = 1
  zero = 0

yanek' :: Words (Vectorspace Universe Double)
yanek' = Words
  (fromList [([Yanek], 1), ([Poet], 0.7), ([Revolutionary], 0.9)])
  [N]
  "Yanek"

dora' :: Words (Vectorspace Universe Double)
dora' = Words (fromList [([Dora], 1), ([Revolutionary], 0.9), ([Poet], 0.3)])
              [N]
              "Dora"

likes' :: Words (Vectorspace Universe Double)
likes' = Words
  (fromList
    [ ([Yanek, IsTrue, Dora]       , 0.9)
    , ([Dora, IsTrue, Yanek]       , 0.8)
    , ([Stepan, IsTrue, Dora]      , 0.6)
    , ([Dora, IsTrue, Poetry]      , 0.8)
    , ([Dora, IsTrue, Chemistry]   , 1)
    , ([Yanek, IsTrue, Poetry]     , 1)
    , ([Yanek, IsTrue, Life]       , 0.9)
    , ([Dora, IsTrue, Life]        , 0.8)
    , ([Stepan, IsTrue, Propaganda], 0.9)
    , ([Stepan, IsTrue, Life]      , 0.1)
    , ([Boris, IsTrue, Life]       , 0.3)
    , ([Boris, IsTrue, Propaganda] , 0.6)
    ]
  )
  [L N, S, R N]
  "likes"

combat' :: Words (Vectorspace Universe Double)
combat' = Words
  (fromList
    [ ([Yanek, IsTrue, Duke]      , 1)
    , ([Yanek, IsTrue, Skouratov] , 0.7)
    , ([Dora, IsTrue, Duke]       , 0.8)
    , ([Dora, IsTrue, Skouratov]  , 0.4)
    , ([Stepan, IsTrue, Duke]     , 1)
    , ([Stepan, IsTrue, Skouratov], 0.9)
    , ([Stepan, IsTrue, Nephew]   , 0.7)
    , ([Boris, IsTrue, Duke]      , 0.9)
    , ([Boris, IsTrue, Nephew]    , 0.1)
    , ([Skouratov, IsTrue, Yanek] , 0.9)
    , ([Skouratov, IsTrue, Stepan], 1)
    ]
  )
  [L N, S, R N]
  "combat"

is' :: Words (Vectorspace Universe Double)
is' = Words
  (fromList
    [ ([Yanek, IsTrue, Revolutionary] , 0.9)
    , ([Yanek, IsTrue, Poet]          , 1)
    , ([Dora, IsTrue, Poet]           , 0.5)
    , ([Dora, IsTrue, Revolutionary]  , 0.7)
    , ([Boris, IsTrue, Revolutionary] , 0.7)
    , ([Stepan, IsTrue, Terrorist]    , 0.95)
    , ([Yanek, IsTrue, Terrorist]     , 0.25)
    , ([Boris, IsTrue, Terrorist]     , 0.25)
    , ([Stepan, IsTrue, Revolutionary], 0.8)
    , ([Duke, IsTrue, Tsarist]        , 1)
    , ([Skouratov, IsTrue, Tsarist]   , 0.9)
    , ([Nephew, IsTrue, Tsarist]      , 0.3)
    , ([Nephew, IsTrue, Innocent]     , 1)
    , ([Yanek, IsTrue, Innocent]      , 0.5)
    ]
  )
  [L N, S, R N]
  "is"

people' :: (Semiring m) => Words (Vectorspace Universe m)
people' = Words
  (fromList
    [ ([Yanek]    , unit)
    , ([Dora]     , unit)
    , ([Stepan]   , unit)
    , ([Duke]     , unit)
    , ([Nephew]   , unit)
    , ([Skouratov], unit)
    , ([Boris]    , unit)
    ]
  )
  [N]
  "people"


yanek = M.singleton yanek'
dora = M.singleton dora'
likes = M.singleton likes'
enjoy = likes
is = M.singleton is'
people = M.singleton people'
combat = M.singleton combat'

who :: (Semiring m) => M.Multiword (Vectorspace Universe m)
who = M.singleton $ Words
  (fromList [ ([a, a, b, a], unit) | a <- universe, b <- universe ])
  [L N, N, R S, N]
  "who"

basis :: (Semiring m) => Universe -> M.Multiword (Vectorspace Universe m)
basis t = M.singleton $ Words (fromList [([t], unit)]) [N] "basis"

tsarist, life, propaganda, poetry, innocent, terrorist
  :: (Semiring m) => M.Multiword (Vectorspace Universe m)
tsarist = basis Tsarist
life = basis Life
propaganda = basis Propaganda
poetry = basis Poetry
innocent = basis Innocent
terrorist = basis Terrorist


revolutionary :: (Semiring m) => M.Multiword (Vectorspace Universe m)
revolutionary =
  M.singleton $ Words (fromList [([Revolutionary], unit)]) [N] "revolutionary"

tsarists :: M.Multiword (Vectorspace Universe Double)
tsarists = (people <> who <> is <> tsarist) M.@@ [N]

revolutionaries :: M.Multiword (Vectorspace Universe Double)
revolutionaries = (people <> who <> is <> revolutionary) M.@@ [N]

that, are :: M.Multiword (Vectorspace Universe Double)
that = who
are = is


-- Tropical semiring. Not used on the examples.
newtype Tropical = Tropical Double deriving (Eq, Show, Num, Ord)
instance Semiring Tropical where
  plus = min
  mult = (+)
  unit = 0
  zero = Tropical $ read "Infinity"

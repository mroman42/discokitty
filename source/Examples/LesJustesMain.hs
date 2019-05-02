module Examples.LesJustesMain where

import           Data.Semigroup
import           Examples.LesJustesUniverse
import           Lambek
import           Models.Rel
import qualified Multiwords                 as M
import           Words

type RelU = Rel Universe

-- Example: Yanek attacks the Duke
yanek :: Words RelU
yanek = Words yanekRel [ N ]
  where
    yanekRel :: RelU
    yanekRel = fromList
      [ [ Yanek ]
      , [ Poet ]
      , [ Alive ]
      , [ Revolutionary ]
      , [ Innocent ]
      ]

attacks :: Words RelU
attacks = Words (fromList
  [ [ Yanek , IsTrue , Duke ]
  , [ Yanek , IsPlot , Duke ]
  ]) [ L N , S , R N ]

duke :: Words RelU
duke = Words dukeRel [ N ]
  where
    dukeRel :: RelU
    dukeRel = fromList
      [ [ Duke ]
      , [ Alive ]
      , [ Tsarist ]
      ]

example2 :: [Words RelU]
example2 = sentence [yanek , kills , duke] @@@ [ S ]


-- Example: Semicartesian verbs
lnot :: Universe -> Words RelU
lnot adjective = Words
  (fromList $ fmap (\x -> [x , x]) $ filter (/= adjective) universe)
  [L N , N]

rnot :: Universe -> Words RelU
rnot adjective = Words
  (fromList $ fmap (\x -> [x , x]) $ filter (/= adjective) universe)
  [N , R N]

cnst :: Universe -> Words RelU
cnst adjective = Words (fromList [[ IsTrue ]]) [ S ]

kills :: Words RelU
kills =
  head $
  sentence [lnot Innocent, cnst IsTrue, rnot Alive]
  @@@ [L N, N, S, N, R N]

example3 :: [Words RelU]
example3 = sentence [ yanek , kills , duke ] @@@ [N , S , N]


-- Example: Grammatical ambiguity (preparation).
nephew :: Words RelU
nephew = Words nephewRel [ N ]
  where
    nephewRel :: RelU
    nephewRel = fromList
      [ [ Nephew ]
      , [ Alive ]
      , [ Tsarist ]
      , [ Innocent ]
      ]

bomb :: Words RelU
bomb = Words
  (fromList [ [Bomb] ])
  [ N ]

and' :: Words RelU
and' = Words
  (fromList $
    [ [ a , a , b ] | a <- universe , b <- universe ] ++
    [ [ a , b , b ] | a <- universe , b <- universe ])
  [L N , N , R N]

example4a :: [Words RelU]
example4a = sentence [ yanek , attacks , duke , and' , nephew ] @@@ [ S ]

using :: Words RelU
using = Words
  (fromList $
    [ [ IsPlot , IsPlot , Bomb ]
    , [ IsTrue , IsTrue , Bomb ]
    ]
  )
  [L S , S , R N]

-- Example: Grammatical ambiguity (full).
with :: M.Multiword RelU
with = M.fromList $
  [ (using , 0.7)
  , (and' , 0.3)
  ]

yanek'   = M.singleton yanek
duke'    = M.singleton duke
nephew'  = M.singleton nephew
bomb'    = M.singleton bomb
using'   = M.singleton using
and''    = M.singleton and'
attacks' = M.singleton attacks

example5a :: M.Multiword RelU
example5a = (yanek' <> attacks' <> duke' <> with <> nephew')  M.@@ [S]

example5b :: M.Multiword RelU
example5b = M.sentence [yanek' , attacks' , duke' , with , bomb' ] M.@@ [S]


-- Example: Meaning in dispute

-- Yanek is a revolutionary.
-- Yanek kills the duke.
-- Is Yanek a saviour?

becomes :: Words RelU
becomes = Words
  (fromList $
     [ [ a ,     a , b ] | a <- universe , b <- universe ] ++
     [ [ Alive , b , b ] | b <- universe ])
  [L N , N , R N]

becomes' :: M.Multiword RelU
becomes' = M.singleton becomes

revolutionary :: M.Multiword RelU
revolutionary = M.fromList $
  [ ( revSaviour , 0.5 )
  , ( revTerrorist , 0.5 )
  ]
  where
    revSaviour = Words (fromList [[Revolutionary] , [Saviour]]) [ N ]
    revTerrorist = Words (fromList [[Revolutionary] , [Terrorist]]) [ N ]

kills''' :: M.Multiword RelU
kills''' =
  M.singleton $ Words
  (fromList $
    [ [ Alive , Terrorist , Innocent , Innocent ] ]
    ++
    [ [ a , a , b , b  ]
       | a <- universe
       , a /= Innocent
       , b <- universe
       , b /= Alive
       , a /= Saviour
       ]
    ++
    [  ])
  [L N, N, N, R N]

discarding' :: M.Multiword RelU
discarding' =
  M.singleton $ Words
  (fromList
    [ [ a ] | a <- universe ])
  [ L N ]


he = M.singleton $ Words (fromList [[a,a] | a <- universe]) [L N , N]
saviour = M.singleton $ Words (fromList [[Saviour]]) [N]
terrorist = M.singleton $ Words (fromList [[Terrorist]]) [N]
alive = M.singleton $ Words (fromList [[Alive]]) [N]
is' = M.singleton $ Words (fromList [[a,a] | a <- universe]) [L N , N]
(?) = M.singleton $ Words (fromList [[a,a] | a <- universe]) [L N , L N]

example6 :: M.Multiword RelU
example6 = (sentence1 <> sentence2 <> sentence3) M.@@ []
  where
    sentence1 = (yanek' <> becomes' <> revolutionary) M.@@ [N]
    sentence2 = (he <> kills''' <> duke' <> discarding') M.@@ [L N , N]
    sentence3 = (is' <> he <> terrorist <> (?)) M.@@ [L N]

example7 :: M.Multiword RelU
example7 = (sentence1 <> sentence2 <> sentence3) M.@@ []
  where
    sentence1 = (yanek' <> becomes' <> revolutionary) M.@@ [N]
    both      = (duke' <> and'' <> nephew') M.@@ [N]
    sentence2 = (he <> kills''' <> both <> discarding') M.@@ [L N , N]
    sentence3 = (is' <> he <> terrorist <> (?)) M.@@ [L N]

-- Example: Revolutionaries who kill people who is innocent
people :: M.Multiword RelU
people = M.singleton $ Words
  (fromList
    [ [Yanek]
    , [Dora]
    , [Stepan]
    , [Duke]
    , [Skouratov]
    , [Boris]
    , [Nephew]
    ]
  )
  [N]

combat :: M.Multiword RelU
combat = M.singleton $ Words
  (fromList $
     [ [r         , IsTrue , Duke]      | r <- revolutionaries] ++
     [ [r         , IsTrue , Skouratov] | r <- revolutionaries] ++
     [ [Skouratov , IsTrue , r]         | r <- revolutionaries] ++
     [ [Stepan    , IsTrue , Nephew] ])
  [L N , S , R N]
    where
      revolutionaries = [Yanek, Dora, Stepan]

enjoy :: M.Multiword RelU
enjoy = M.singleton $ Words
  (fromList
    [ [Yanek  , IsTrue , Poetry]
    , [Yanek  , IsTrue , Life]
    , [Dora   , IsTrue , Poetry]
    , [Dora   , IsTrue , Chemistry]
    , [Dora   , IsTrue , Life]
    , [Stepan , IsTrue , Propaganda]
    , [Boris  , IsTrue , Propaganda]
    , [Yanek , IsTrue , Dora]
    , [Dora , IsTrue , Yanek]
    , [Stepan , IsTrue , Dora]
    ])
  [L N , S , R N]

is_ :: M.Multiword RelU
is_ = M.singleton $ Words
  (fromList
    [ [Yanek , IsTrue , Revolutionary]
    , [Yanek , IsTrue , Poet]
    , [Dora , IsTrue , Revolutionary]
    , [Stepan , IsTrue , Revolutionary]
    , [Stepan , IsTrue , Terrorist]
    , [Boris , IsTrue , Revolutionary]
    , [Duke  , IsTrue , Tsarist]
    , [Skouratov  , IsTrue , Tsarist]
    , [Nephew  , IsTrue , Innocent]
    ])
  [L N , S , R N]

who :: M.Multiword RelU
who = M.singleton $ Words
  (fromList [ [a , a , b , a ] | a <- universe , b <- universe ])
  [ L N , N , R S , N ]

tsarist :: M.Multiword RelU
tsarist = M.singleton $ Words (fromList [[Tsarist]]) [N]

tsarists :: M.Multiword RelU
tsarists = (people <> who <> is_ <> tsarist) M.@@ [N]

revolutionaries :: M.Multiword RelU
revolutionaries = (people <> who <> is_ <> revolutionary) M.@@ [N]

example8 :: M.Multiword RelU
example8 = (people <> who <> combat <> tsarists) M.@@ [N]

example9 :: M.Multiword RelU
example9 = (people <> who <> combat <> people <> who <> combat <> tsarists) M.@@ [N]

-- Example: Revolutionaries who enjoy life enjoy propaganda
life :: M.Multiword RelU
life = M.singleton $ Words (fromList [[Life]]) [N]

propaganda :: M.Multiword RelU
propaganda = M.singleton $ Words (fromList [[Propaganda]]) [N]

innocent :: M.Multiword RelU
innocent = M.singleton $ Words (fromList [[Innocent]]) [N]

poetry :: M.Multiword RelU
poetry = M.singleton $ Words (fromList [[Poetry]]) [N]

chemistry :: M.Multiword RelU
chemistry = M.singleton $ Words (fromList [[Chemistry]]) [N]

example10 :: M.Multiword RelU
example10 = (revolutionaries <> who <> enjoy <> life <> enjoy <> propaganda) M.@@ [S]

example11 :: M.Multiword RelU
example11 = (yanek' <> likes <> revolutionaries <> who <> enjoy <> poetry <> or <> chemistry) M.@@ [S]
  where
    likes = enjoy
    or = and''

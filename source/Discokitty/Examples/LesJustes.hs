module Discokitty.Examples.LesJustes where

import           Discokitty
import           Discokitty.Examples.LesJustesUniverse
import           Discokitty.Models.Rel


type RelU = Rel Universe

yanek :: Words RelU
yanek = Words yanekRel [N] "Yanek"
 where
  yanekRel :: RelU
  yanekRel = fromList [[Yanek], [Poet], [Alive], [Revolutionary], [Innocent]]

combat :: Words RelU
combat = Words
  (  fromList
  $  [ [r, IsTrue, Duke] | r <- revolutionaries ]
  ++ [ [r, IsTrue, Skouratov] | r <- revolutionaries ]
  ++ [ [Skouratov, IsTrue, r] | r <- revolutionaries ]
  ++ [[Stepan, IsTrue, Nephew]]
  )
  [L N, S, R N]
  "combat"
  where revolutionaries = [Yanek, Dora, Stepan]

revolutionary :: Words RelU
revolutionary = Words (relation [[Revolutionary]]) [N] "revolutionary"

tsarist :: Words RelU
tsarist =
  Words {meaning = relation [[Tsarist]], grammar = [N], text = "tsarist"}


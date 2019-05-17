module Discokitty.Models.Convex where

import           Discokitty.Models.Rel

data Convex u = Convex (Rel (u , Int))

subdivisions :: Int
subdivisions = 10

fromList :: (Ord u) => 


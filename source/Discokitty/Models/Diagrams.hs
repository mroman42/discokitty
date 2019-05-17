{-|
Module: Diagrams
Description: A category whose morphisms are tikz diagrams.
License: GPL-3

This module allows us to obtain diagrams for our sentences. In order
to obtain these, we define a monoidal category whose morphisms are
diagrams and then implement a cup operation on that category that
consists on wiring two of them together.  We apply then the ideas of
DisCoCat to words whose meanings are themselves diagrams, and just
perform the necessary reductions.
|-}

module Discokitty.Models.Diagrams
  ( tikzDiagrams
  )
where


import           Discokitty.HasCups
import           Discokitty.Words

-- | A node on a tikzpicture represents one particular position. It
-- is labeled with some text and has a particular geometric style.
data Node = Node
  { idNumber :: Int
  , xPos     :: Double
  , yPos     :: Double
  , label    :: String
  , style    :: Style
  }
  deriving (Show)

-- | Geometric styles for the nodes. A copoint will be a state in a
-- monoidal category represented by a triangle.
data Style = Copoint | None

instance Show Style where
  show Copoint = "wide copoint"
  show None    = "none"

type NodeId = Int

data Wire = Wire
  { looseness :: Double
  , from      :: NodeId
  , to        :: NodeId
  }
  deriving (Show)

-- | A diagram is given by some nodes and some wires between them.
data Diagram = Diagram
  { wires :: [Wire]
  , nodes :: [Node]
  }
  deriving (Show)

-- | A schema is an abstract diagram with some nodes and wires but
-- also with a fixed number of words.  The difference with a diagram
-- is that a Schema must only contain wire nodes and no word nodes.
-- That is, a Schema does not contain triangles representing states.
data Schema = Schema
  { schemaNodes       :: [Node]
  , schemaUnusedNodes :: [Node]
  , schemaWires       :: [Wire]
  , nWords            :: Int
  }
  deriving (Show)

nodeWidth :: Double
nodeWidth = 4

generateNodes :: Int -> Words m -> Words Schema
generateNodes offset w = Words
  { meaning = Schema
    { schemaNodes = createNode <$> [0 .. (len - 1)]
    , schemaUnusedNodes = createNode <$> [0 .. (len - 1)]
    , schemaWires = []
    , nWords      = 1
    }
  , grammar = grammar w
  , text    = ""
  }
 where
  createNode :: Int -> Node
  createNode n = Node
    { idNumber = offset + n
    , xPos     = ((fromIntegral n + 1) * nodeWidth / (fromIntegral len + 1))
      - (nodeWidth / 2.0)
    , yPos     = 0
    , label    = ""
    , style    = None
    }

  len :: Int
  len = length $ grammar w

schemaWords :: [Words m] -> [Words Schema]
schemaWords ws = (generateNodes . length) ws <$> ws

shiftNodeId :: Int -> Schema -> Schema
shiftNodeId n b = b
  { schemaNodes = fmap (\p -> p { idNumber = n + idNumber p }) $ schemaNodes b
  , schemaUnusedNodes = fmap (\p -> p { idNumber = n + idNumber p }) $ schemaUnusedNodes b
  }

shiftNodePos :: Int -> Schema -> Schema
shiftNodePos p b = b
  { schemaNodes = fmap (\q -> q { xPos = 4.0 * (fromIntegral p) + xPos q }) $ schemaNodes b
  , schemaUnusedNodes = fmap (\q -> q { xPos = 4.0 * (fromIntegral p) + xPos q }) $ schemaUnusedNodes b
  }

shiftWiresId :: Int -> Schema -> Schema
shiftWiresId n b = b
  { schemaWires = fmap (\q -> q { from = n + from q, to = n + to q })
    $ schemaWires b
  }

schemaCup :: Int -> Schema -> Schema -> Schema
schemaCup n a b = joinSchemas a (shifted b)
 where
  shifted :: Schema -> Schema
  shifted =
    shiftNodePos (nWords a)
      . shiftWiresId (length (schemaNodes a))
      . shiftNodeId (length (schemaNodes a))

  joinSchemas :: Schema -> Schema -> Schema
  joinSchemas u v = Schema
    { schemaNodes       = schemaNodes u ++ schemaNodes v
    , schemaUnusedNodes = reverse (drop n (reverse (schemaNodes u))) ++ drop n (schemaNodes v)
    , nWords            = nWords u + nWords v
    , schemaWires = schemaWires u
       ++ schemaWires v
       ++ ( fmap
             (\(m, x, y) -> Wire
               { looseness = 1.25
               , from = x
               , to = y
               }
             )
         $ zip3 [0 .. (n - 1)]
                (reverse (idNumber <$> schemaUnusedNodes u))
                (idNumber <$> schemaUnusedNodes v) )
    }

schemaUnit :: Schema
schemaUnit = Schema
  { schemaNodes = []
  , schemaUnusedNodes = []
  , schemaWires = []
  , nWords = 0
  }

instance HasCups Schema where
  cup = schemaCup
  cunit = schemaUnit

tikzDiagrams :: [Words m] -> String
tikzDiagrams = unlines . fmap generateTikz . textDiagrams

textDiagrams :: [Words m] -> [Diagram]
textDiagrams ws = do
  solution <- sentence $ schemaWords ws
  let textWires = schemaWires $ meaning solution
  let textNodes = fmap (\ n -> n { yPos = yPos n - 0.5 }) $ schemaNodes $ meaning solution
  let openWires = danglingWires $ meaning solution
  let openNodes = danglingNodes $ meaning solution
  return Diagram
    { nodes = allWordNodes ++ textNodes ++ openNodes
    , wires = textWires ++ openWires
    }
  where

    -- We need some word nodes that will generate triangles on the
    -- final diagram.
    allWordNodes :: [Node]
    allWordNodes = numberedNode <$> zip [0 ..] ws

    -- Numbered nodes.
    numberedNode :: (Int, Words m) -> Node
    numberedNode (n, w) = Node
      { idNumber = n
      , xPos     = fromIntegral n * 4
      , yPos     = 0
      , label    = text w
      , style    = Copoint
      }

    danglingWires :: Schema -> [Wire]
    danglingWires s = do
      (f, t) <- zip (idNumber <$> preDanglingNodes s)
                (idNumber <$> danglingNodes s)
      return Wire {from = f, to = t, looseness = 0}

    danglingNodes :: Schema -> [Node]
    danglingNodes s =
      (\p -> p { yPos = yPos p - 3, idNumber = idNumber p + 100 })
      <$> preDanglingNodes s

    preDanglingNodes :: Schema -> [Node]
    preDanglingNodes s =
      filter (not . (`elem` occupiedIds s) . idNumber) (schemaNodes s)

    occupiedIds :: Schema -> [NodeId]
    occupiedIds s = (from <$> schemaWires s) ++ (to <$> schemaWires s)

generateTikz :: Diagram -> String
generateTikz diagram =
  unlines
    $  ["\\begin{tikzpicture}", "\\begin{pgfonlayer}{nodelayer}"]
    ++ fmap generateNode (nodes diagram)
    ++ ["\\end{pgfonlayer}{nodelayer}", "\\begin{pgfonlayer}{edgelayer}"]
    ++ fmap generateWire (wires diagram)
    ++ ["\\end{pgfonlayer}", "\\end{tikzpicture}"]
 where
  generateNode :: Node -> String
  generateNode node =
    "\\node "
      ++ "[style="
      ++ show (style node)
      ++ "] "
      ++ "("
      ++ show (idNumber node)
      ++ ") "
      ++ "at ("
      ++ show (xPos node)
      ++ ", "
      ++ show (yPos node)
      ++ ") "
      ++ "{"
      ++ label node
      ++ "};"

  generateWire :: Wire -> String
  generateWire wire =
    "\\draw ["
      ++ "bend right=90, "
      ++ "looseness="
      ++ (show . looseness) wire
      ++ "] "
      ++ "("
      ++ (show . from) wire
      ++ ".center) to ("
      ++ (show . to) wire
      ++ ".center);"

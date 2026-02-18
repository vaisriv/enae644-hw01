module Graphs
  ( Graph (..),
    Node (..),
    Edge (..),
    readGraph,
    printGraph,
    astar,
  )
where

import Data.Char (isSpace)
import Data.Heap (Entry (..))
import qualified Data.Heap as Heap
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
import System.FilePath ((</>))

-- graph structures
data Node = Node
  { nodeID :: Int,
    xCoord :: Float,
    yCoord :: Float
  }
  deriving (Show)

data Edge = Edge
  { startNode :: Int,
    endNode :: Int,
    weight :: Maybe Float
  }
  deriving (Show)

data Graph = Graph
  { nodes :: [Node],
    edges :: [Edge]
  }
  deriving (Show)

-- graph IO
readGraph :: FilePath -> Bool -> IO Graph
readGraph graphpath weighted = do
  nodesContent <- readFile (graphpath </> "nodes.txt")
  edgesContent <- readFile (graphpath </> edgesFile)
  let graph =
        Graph
          { nodes = parseNodes nodesContent,
            edges = parseEdges weighted edgesContent
          }
  return graph
  where
    edgesFile = if weighted then "edges_with_costs.txt" else "edges.txt"

printGraph :: Graph -> IO ()
printGraph graph = do
  putStrLn "Nodes:"
  mapM_ printNode (nodes graph)
  putStrLn "Edges:"
  mapM_ printEdge (edges graph)
  where
    printNode n =
      putStrLn $
        "  Node "
          ++ show (nodeID n)
          ++ " at ("
          ++ show (xCoord n)
          ++ ", "
          ++ show (yCoord n)
          ++ ")"
    printEdge e =
      putStrLn $
        "  Edge "
          ++ show (startNode e)
          ++ " -> "
          ++ show (endNode e)
          ++ weightStr (weight e)
    weightStr Nothing = ""
    weightStr (Just w) = " (weight: " ++ show w ++ ")"

-- parsing
parseRow :: String -> [String]
parseRow = map trim . splitOn ','
  where
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
    splitOn _ "" = [""]
    splitOn delim str =
      let (field, rest) = break (== delim) str
       in field : case rest of
            [] -> []
            (_ : xs) -> splitOn delim xs

parseNodes :: String -> [Node]
parseNodes content =
  map parseNode . tail . lines $ content
  where
    parseNode line =
      let [idStr, xStr, yStr] = parseRow line
       in Node
            { nodeID = read idStr,
              xCoord = read xStr,
              yCoord = read yStr
            }

parseEdges :: Bool -> String -> [Edge]
parseEdges weighted content =
  map parseEdge . tail . lines $ content
  where
    parseEdge line
      | weighted =
          let [startStr, endStr, weightStr] = parseRow line
           in Edge
                { startNode = read startStr,
                  endNode = read endStr,
                  weight = Just (read weightStr)
                }
      | otherwise =
          let [startStr, endStr] = parseRow line
           in Edge
                { startNode = read startStr,
                  endNode = read endStr,
                  weight = Nothing
                }

-- A*
-- euclidean distance heuristic between two nodes
heuristic :: Node -> Node -> Float
heuristic a b =
  let dx = xCoord a - xCoord b
      dy = yCoord a - yCoord b
   in sqrt (dx * dx + dy * dy)

-- look up a node by ID
nodeByID :: Graph -> Int -> Node
nodeByID g nid =
  case filter (\n -> nodeID n == nid) (nodes g) of
    (n : _) -> n
    [] -> error $ "Node " ++ show nid ++ " not found in graph"

-- build adjacency map
buildAdjacency :: Graph -> Map Int [(Int, Float)]
buildAdjacency g = foldr addEdge Map.empty (edges g)
  where
    addEdge e acc =
      let w = fromMaybe 1.0 (weight e)
          src = startNode e
          dst = endNode e
       in Map.insertWith (++) src [(dst, w)] acc

-- run A* on the graph from startID to goalID.
-- returns Just (cost, path) on success, Nothing if no path exists
-- `path` is the list of node IDs from start to goal, inclusive
astar :: Graph -> Int -> Int -> Maybe (Float, [Int])
astar g startID goalID = go initOpen initCostSoFar initCameFrom
  where
    adjacency = buildAdjacency g
    goalNode = nodeByID g goalID

    -- each heap entry: priority = f = g + h, payload = current nodeID
    initH = heuristic (nodeByID g startID) goalNode
    initOpen = Heap.singleton (Entry initH startID)
    initCostSoFar = Map.singleton startID 0.0
    initCameFrom = Map.empty :: Map Int Int

    go open costSoFar cameFrom
      | Heap.null open = Nothing -- exhausted without reaching goal
      | otherwise =
          let Entry _ current = Heap.minimum open
              open' = Heap.deleteMin open
           in if current == goalID
                then Just (costSoFar Map.! goalID, reconstructPath cameFrom goalID)
                else
                  let neighbours = fromMaybe [] (Map.lookup current adjacency)
                      (open'', costSoFar', cameFrom') =
                        foldr (relax current) (open', costSoFar, cameFrom) neighbours
                   in go open'' costSoFar' cameFrom'

    relax current (nbr, w) (openAcc, costAcc, cameAcc) =
      let tentative = (costAcc Map.! current) + w
       in case Map.lookup nbr costAcc of
            Just existing | existing <= tentative -> (openAcc, costAcc, cameAcc)
            _ ->
              let h = heuristic (nodeByID g nbr) goalNode
                  f = tentative + h
                  openAcc' = Heap.insert (Entry f nbr) openAcc
                  costAcc' = Map.insert nbr tentative costAcc
                  cameAcc' = Map.insert nbr current cameAcc
               in (openAcc', costAcc', cameAcc')

    reconstructPath cameFrom nid =
      case Map.lookup nid cameFrom of
        Nothing -> [nid]
        Just parent -> reconstructPath cameFrom parent ++ [nid]

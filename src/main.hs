module Main where

import qualified Graphs (Graph, astar, printGraph, readGraph)
import qualified System.Environment
import qualified System.Exit

data Args = Args
  { graphPath :: FilePath,
    weighted :: Bool,
    startID :: Int,
    goalID :: Int
  }

main :: IO ()
main = do
  args <- System.Environment.getArgs >>= parse
  graph <- Graphs.readGraph (graphPath args) (weighted args)
  Graphs.printGraph graph
  putStrLn ""
  putStrLn $
    "Searching from node "
      ++ show (startID args)
      ++ " to node "
      ++ show (goalID args)
  case Graphs.astar graph (startID args) (goalID args) of
    Nothing -> putStrLn "No path found."
    Just (cost, path) -> do
      putStrLn $ "Path found (cost: " ++ show cost ++ "):"
      putStrLn $ "  " ++ unwords (map show path)

parse :: [String] -> IO Args
parse ["-h"] = usage >> exit >> return (Args "" False 0 0)
parse ["-v"] = version >> exit >> return (Args "" False 0 0)
parse ["-w", path, startStr, goalStr] = return $ Args path True (read startStr) (read goalStr)
parse [path, startStr, goalStr] = return $ Args path False (read startStr) (read goalStr)
parse [] = putStrLn "No input provided." >> die >> return (Args "" False 0 0)
parse _ = putStrLn "Usage error." >> die >> return (Args "" False 0 0)

usage :: IO ()
usage = putStrLn "Usage: enae644-hw01 [-vhw] [graphpath] [start] [goal]"

version :: IO ()
version = putStrLn "Haskell enae644-hw01 0.1"

exit :: IO ()
exit = System.Exit.exitWith System.Exit.ExitSuccess

die :: IO ()
die = System.Exit.exitWith (System.Exit.ExitFailure 1)

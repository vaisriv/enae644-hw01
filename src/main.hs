module Main where

import qualified Graphs (Graph, printGraph, readGraph)
import qualified System.Environment
import qualified System.Exit

main = do
  (graphpath, weighted) <- System.Environment.getArgs >>= parse
  graph <- Graphs.readGraph graphpath weighted
  Graphs.printGraph graph

parse :: [String] -> IO (FilePath, Bool)
parse ["-h"] = usage >> exit >> return ("", False)
parse ["-v"] = version >> exit >> return ("", False)
parse [graphpath] = return (graphpath, False)
parse ["-w", graphpath] = return (graphpath, True)
parse [graphpath, "-w"] = return (graphpath, True)
parse [] = putStrLn "No input provided." >> die >> return ("", False)
parse _ = putStrLn "Too many arguments." >> die >> return ("", False)

usage = putStrLn "Usage: enae644-hw01 [-vhw] [graphpath ..]"

version = putStrLn "Haskell enae644-hw01 0.1"

exit = System.Exit.exitWith System.Exit.ExitSuccess

die = System.Exit.exitWith (System.Exit.ExitFailure 1)

module Main where

import qualified Palindrome (isPalindrome)
import qualified System.Environment
import qualified System.Exit

main = do
  text <- System.Environment.getArgs >>= parse
  let response =
        if Palindrome.isPalindrome text
          then "it is"
          else "it is not"
  putStrLn response

parse ["-h"] = usage >> exit
parse ["-v"] = version >> exit
parse [text] = return text
parse [] = putStrLn "No input provided." >> die >> return ""
parse _ = putStrLn "Too many arguments." >> die >> return ""

usage = putStrLn "Usage: enae644-hw01 [-vh] [text ..]"

version = putStrLn "Haskell enae644-hw01 0.1"

exit = System.Exit.exitWith System.Exit.ExitSuccess

die = System.Exit.exitWith (System.Exit.ExitFailure 1)

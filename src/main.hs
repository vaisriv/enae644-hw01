module Main where
import qualified Palindrome

main :: IO ()
main = do
  putStrLn "enter a word, and i'll let you know if it is a palindrome"
  text <- getLine
  let response =
        if Palindrome.isPalindrome text
          then "it is!"
          else "it is not!"
  putStrLn response

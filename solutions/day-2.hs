module Main where

import Data.Maybe (fromMaybe)
import System.Environment (getArgs)
import Text.Regex.Applicative (Alternative (many), anySym, string, sym, (=~))
import Text.Regex.Applicative.Common (decimal)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}

data Rule = Rule {atLeast, atMost :: Int, symbol :: Char}

data Password = Password Rule String

type Input = [Password] -- default to Bytestring, but very likely you'll need to change it

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser = fromMaybe (error "no parse") . traverse parse . lines
  where
    parse = (=~ password)
    password = Password <$> rule <*> many anySym
    rule = Rule <$> decimal <* sym '-' <*> decimal <* sym ' ' <*> anySym <* string ": "

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = length . filter valid
  where
    valid (Password (Rule lo hi char) pass) = numOccurs >= lo && numOccurs <= hi
      where
        numOccurs = length $ filter (== char) pass

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = length . filter valid
  where
    valid (Password (Rule lo hi char) pass) = (pass !! (lo - 1) == char && pass !! (hi - 1) /= char) || (pass !! (lo - 1) /= char && pass !! (hi - 1) == char)

main :: IO ()
main = do
  -- run this with cabal run -- day-x <part-number> <file-to-solution>
  -- example: cabal run -- day-3 2 "./input/day-3.example"
  -- will run part two of day three with input file ./input/day-3.example
  [part, filepath] <- getArgs
  input <- parser <$> readFile filepath -- use parser <$> readFile filepath if String is better
  if read @Int part == 1
    then do
      putStrLn "solution to problem 1 is:"
      print $ solve1 input
    else do
      putStrLn "solution to problem 2 is:"
      print $ solve2 input

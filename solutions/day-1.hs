module Main where

import Control.Monad (replicateM)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (getArgs)

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = [Int] -- default to Bytestring, but very likely you'll need to change it

type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser = map read . lines

entriesSatisfying :: Int -> ([a] -> Bool) -> [a] -> [a]
entriesSatisfying n f xs = fromMaybe (error "no entries satisfying the predicate") $ find f (replicateM n xs)

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = product <$> entriesSatisfying 2 ((== 2020) . sum)

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 = product <$> entriesSatisfying 3 ((== 2020) . sum)

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

module Main where

import Data.Set (Set)
import Data.Set qualified as S
import System.Environment (getArgs)
import Control.Arrow ((&&&))

{- Types for your input and your solution

- Input    should as the type of your input parameter. AOC, typically uses arrays, matrices or complex data structures.
- Solution should be the type of your solution. Typically is an Int, but It can be other things, like a list of numbers
         or a list of characters
-}
type Input = (Int, [Set Int]) -- default to Bytestring, but very likely you'll need to change it
data Slope = Slope {right, down :: Int}
type Solution = Int

-- | parser transforms a raw bytestring (from your ./input/day-X.input) to your Input type.
--   this is intended to use attoparsec for such a transformation. You can use Prelude's
--   String if it fit better for the problem
parser :: String -> Input
parser = (length . head &&& map readTrees) . lines
  where
    readTrees = S.fromList . map fst . filter ((== '#') . snd) . zip [0 ..]

hitTree :: Int -> Int -> Set Int -> Bool
hitTree width x = S.member (x `mod` width)

treeCount :: Input -> Slope -> Int
treeCount (width, trees) (Slope r d) = length 
              . filter (uncurry (hitTree width)) 
              . zip [0,r..] 
              $ takeNth d trees
              where takeNth _ [] = []
                    takeNth n xs@(x:_) = x : takeNth n (drop n xs)

-- | The function which calculates the solution for part one
solve1 :: Input -> Solution
solve1 = (`treeCount` Slope 3 1)

-- | The function which calculates the solution for part two
solve2 :: Input -> Solution
solve2 input = product . map (treeCount input) $ [Slope 1 1
                                                , Slope 3 1
                                                , Slope 5 1
                                                , Slope 7 1
                                                , Slope 1 2
                                                ] 

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

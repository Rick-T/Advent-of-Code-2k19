module Main where

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15

import Control.Monad (forM_)
import System.Environment (getArgs)
import System.TimeIt (timeItNamed)

main :: IO ()
main = do
  puzzles <- fmap read <$> getArgs
  forM_ puzzles runPuzzle

runPuzzle :: Int -> IO ()
runPuzzle p = do
  let (part1, part2) = getFuncs p
  runFirst p part1
  runSecond p part2

getFuncs :: Int -> (IO (), IO ())
getFuncs 1  = printable (Day01.part1, Day01.part2)
getFuncs 2  = printable (Day02.part1, Day02.part2)
getFuncs 3  = printable (Day03.part1, Day03.part2)
getFuncs 4  = printable (return Day04.part1, return Day04.part2)
getFuncs 5  = printable (Day05.part1, Day05.part2)
getFuncs 6  = printable (Day06.part1, Day06.part2)
getFuncs 7  = printable (Day07.part1, Day07.part2)
getFuncs 8  = printable (Day08.part1, Day08.part2)
getFuncs 9  = printable (Day09.part1, Day09.part2)
getFuncs 10 = printable (Day10.part1, Day10.part2)
getFuncs 11 = printable (Day11.part1, Day11.part2)
getFuncs 12 = printable (return Day12.part1, return Day12.part2)
getFuncs 13 = printable (Day13.part1, Day13.part2)
getFuncs 14 = printable (Day14.part1, Day14.part2)
getFuncs 15 = printable (Day15.part1, Day15.part2)

printable :: (Show a, Show b) => (IO a, IO b) -> (IO (), IO ())
printable (a, b) = (print =<< a, print =<< b)

runFirst :: Int -> IO () -> IO ()
runFirst p = timeItNamed $ "Puzzle " ++ show p ++ " part 1"

runSecond :: Int -> IO () -> IO ()
runSecond p = timeItNamed $ "Puzzle " ++ show p ++ " part 2"

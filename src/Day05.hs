module Day05 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.Trans.RWS.Lazy

part1 :: IO [Int]
part1 = snd . evalRWS runProgram 1 <$> initComputer <$> loadMemory "input/Day05.txt"

part2 :: IO Int
part2 = head . snd . evalRWS runProgram 5 <$> initComputer <$> loadMemory "input/Day05.txt"

module Day09 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.Trans.RWS.Lazy

part1 :: IO [Integer]
part1 = snd . evalRWS runProgram 1 <$> initComputer <$> loadMemory "input/Day09.txt"

part2 :: IO Integer
part2 = head . snd . evalRWS runProgram 2 <$> initComputer <$> loadMemory "input/Day09.txt"

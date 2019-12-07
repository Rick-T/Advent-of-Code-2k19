module Day05 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.Trans.RWS.Lazy

solution1 :: IO [Int]
solution1 = snd . evalRWS runProgram 1 <$> initComputer <$> loadMemory "input/Day05/input01.txt"

solution2 :: IO Int
solution2 = head . snd . evalRWS runProgram 5 <$> initComputer <$> loadMemory "input/Day05/input01.txt"

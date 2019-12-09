module Day02 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.Trans.RWS.Lazy

part1 :: IO Integer
part1 = fst . evalRWS (runProgram *> readAddr 0) 1 <$> initComputer <$> initMemory 12 2 <$> loadMemory "input/Day02.txt"

part2 :: IO Integer
part2 = (\m -> head [ 100 * x + y | x <- [0 .. 99], y <- [0 .. 99], (fst . evalRWS (runProgram *> readAddr 0) 1 $ initComputer $ initMemory x y m) == 19690720]) <$> loadMemory "input/Day02.txt"
module Day02 ( part1, part2 ) where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.Trans.RWS.Lazy

part1 :: IO Integer
part1 = solve 12 2 <$> loadComputer "input/Day02.txt"

part2 :: IO Integer
part2 = (\c -> head [ 100 * x + y | x <- [0 .. 99], y <- [0 .. 99], (solve x y c) == 19690720]) <$> loadComputer "input/Day02.txt"

solve :: Integer -> Integer -> Computer -> Integer
solve noun verb c = fst $ evalRWS (compute noun verb) 1 c

compute :: Integer -> Integer -> ComputerState Integer
compute noun verb = writeAddr 1 noun *> writeAddr 2 verb *> runProgram *> readAddr 0

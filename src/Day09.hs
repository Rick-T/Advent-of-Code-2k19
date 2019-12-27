module Day09 (part1, part2) where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.RWS.Strict

part1 :: IO [Val]
part1 = solve 1

part2 :: IO Val
part2 = head <$> solve 2

solve :: Val -> IO [Val]
solve input = snd . evalRWS runProgram input <$> loadComputer "input/Day09.txt"

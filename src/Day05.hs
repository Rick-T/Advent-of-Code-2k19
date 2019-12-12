module Day05 (part1, part2) where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.RWS.Lazy

part1 :: IO [Val]
part1 = solve 1

part2 :: IO Val
part2 = head <$> solve 5

solve :: Val -> IO [Val]
solve input = snd . evalRWS runProgram input <$> loadComputer "input/Day05.txt"

module Day09 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.Trans.RWS.Lazy

part1 :: IO [Integer]
part1 = solve 1

part2 :: IO Integer
part2 = head <$> solve 2

solve :: Integer -> IO [Integer]
solve input = snd . evalRWS runProgram input <$> loadComputer "input/Day09.txt"

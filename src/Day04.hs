module Day04 where

import Paths_Advent_of_Code_2k19
import Common.Util ( toNumber )
import Data.List

type Password = [Int]

part1 :: Int
part1 = length $ filter (\x -> isInRange x && hasGroupOf2 x) candidates

part2 :: Int
part2 = length $ filter (\x -> isInRange x && hasGroupOfExactly2 x) candidates

candidates :: [Password]
candidates = [ [a, b, c, d, e, f] | a <- [1 .. 6], b <- [a .. 9], c <- [b .. 9], d <- [c .. 9], e <- [d .. 9], f <- [e .. 9] ]

isInRange :: Password -> Bool
isInRange = (\x -> x >= passMin && x <= passMax) . toNumber

hasGroupOf2 :: Password -> Bool
hasGroupOf2 = any (\x -> length x >= 2) . group

hasGroupOfExactly2 :: Password -> Bool
hasGroupOfExactly2 = any (\x -> length x == 2) . group

passMin :: Int
passMin = 197487

passMax :: Int
passMax = 673251
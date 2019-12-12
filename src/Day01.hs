module Day01 (part1, part2) where

import Paths_Advent_of_Code_2k19

type Mass = Int

type Fuel = Int

part1 :: IO Int
part1 = sum . fmap fuelForModule <$> moduleMasses

part2 :: IO Int
part2 = sum . fmap completeFuelForModule <$> moduleMasses

fuelForModule :: Mass -> Fuel
fuelForModule x = x `div` 3 - 2

completeFuelForModule :: Mass -> Fuel
completeFuelForModule x = let ffm = fuelForModule x in if ffm < 0 then 0 else ffm + completeFuelForModule ffm

moduleMasses :: IO [Mass]
moduleMasses = fmap read . lines <$> (readFile =<< getDataFileName "input/Day01.txt")

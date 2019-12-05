module Day01 where

import Paths_Advent_of_Code_2k19

type Mass = Int

type Fuel = Int

solveDay01 :: IO Int
solveDay01 = sum . fmap fuelForModule <$> moduleMasses

solveDay01Again :: IO Int
solveDay01Again = sum . fmap completeFuelForModule <$> moduleMasses

fuelForModule :: Mass -> Fuel
fuelForModule x = x `div` 3 - 2

completeFuelForModule :: Mass -> Fuel
completeFuelForModule x = let
    ffm = fuelForModule x
    in
        if ffm < 0 then 0 else ffm + completeFuelForModule ffm

moduleMasses :: IO [Mass]
moduleMasses = fmap read . lines <$> (readFile =<< getDataFileName "input/Day01/input01.txt")
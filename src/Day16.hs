module Day16 where

import Paths_Advent_of_Code_2k19
import Common.Util

part1 :: IO Int
part1 = toNumber . Prelude.take 8 . (!! 100) . fft <$> loadInput

part2 :: IO Int
part2 = toNumber . take 8 . (!! 100) . iterate stepPhase <$> offsetInput

stepPhase :: [Int] -> [Int]
stepPhase = fmap (`mod` 10) . init . scanr (+) 0

fft :: [Int] -> [[Int]]
fft = iterate fftPhase

fftPhase :: [Int] -> [Int]
fftPhase inputs = positionalTransform inputs <$> [1 .. length inputs]

positionalTransform :: [Int] -> Int -> Int
positionalTransform input p = (`mod` 10) $ abs $ sum $ zipWith (*) input $ positionalPattern p

positionalPattern :: Int -> [Int]
positionalPattern p = drop 1 $ cycle $ foldMap (replicate p) basePattern

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

offsetInput :: IO [Int]
offsetInput = do
    input <- realInput
    return $ drop (offset input) input

offset :: [Int] -> Int
offset input = toNumber $ take 7 input

realInput :: IO [Int]
realInput = concat . replicate 10000 <$> loadInput

loadInput :: IO [Int]
loadInput = fmap (read . return) <$> (readFile =<< getDataFileName "input/Day16.txt")

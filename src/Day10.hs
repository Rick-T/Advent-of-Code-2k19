module Day10 where

import Paths_Advent_of_Code_2k19
import Control.Monad.Reader
import Data.List
import Data.Ord
import Data.Ratio

type Position = (Int, Int)

type Angle = Double

type AsteroidField = [Position]

part1 :: IO Int
part1 = do
  asteroids <- readAsteroids
  let p = bestPosition asteroids
  return $ length (asteroidClassesForPosition asteroids p)

part2 :: IO Int
part2 = do
  asteroids <- readAsteroids
  let laserPos = bestPosition asteroids
  let classes = fmap (sortBy (comparing (distance laserPos)))
              $ sortBy (comparing (angle laserPos . head))
              $ asteroidClassesForPosition asteroids laserPos
  return $ (\(x, y) -> 100*x + y) $ shoot 200 classes

shoot :: Int -> [[Position]] -> Position
shoot i cs
  | i <= length cs = head $ cs !! (i - 1)
  | otherwise = shoot (i - length cs) $ filter (\l -> not $ null l) $ fmap tail $ cs

bestPosition :: AsteroidField -> Position
bestPosition asteroids = maximumBy (comparing $ length . asteroidClassesForPosition asteroids) $ asteroids

asteroidClassesForPosition :: AsteroidField -> Position -> [[Position]]
asteroidClassesForPosition asteroids p = equivalenceClasses (angle p) $ delete p $ asteroids

distance :: Position -> Position -> Double
distance (a, b) (x, y) = sqrt $ fromIntegral ((a-x)^2 + (b-y)^2)

angle :: Position -> Position -> Angle
angle (x, y) (a, b) = pi - (atan2 (fromIntegral $ a - x) (fromIntegral $ b - y))

equivalenceClasses :: (Ord b) => (a -> b) -> [a] -> [[a]]
equivalenceClasses f as = groupBy (\a b ->  f a == f b) $ sortBy (comparing f) as

isAsteroid :: Char -> Bool
isAsteroid '#' = True
isAsteroid '.' = False

readAsteroids :: IO AsteroidField
readAsteroids = do
  content <- readFile =<< getDataFileName "input/Day10.txt"
  return $ concat
         $ fmap (fmap fst)
         $ fmap (filter (\(_, c) -> isAsteroid c))
         $ zipWith (\y l -> fmap (\(x, c) -> ((x, y), c)) l) [0..]
         $ fmap (zip [0..])
         $ lines content

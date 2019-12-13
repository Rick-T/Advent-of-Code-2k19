module Day12 (part1, part2) where

import Paths_Advent_of_Code_2k19
import Common.Vector

type Position = Vector Int

type Velocity = Vector Int

type Acceleration = Vector Int

type Body = (Position, Velocity)

part1 :: Int
part1 = sum $ fmap energy $ iterate stepTime moons !! 1000

part2 :: Int
part2 = foldr lcm 1 $ period <$> traverse invert moons

period :: (Eq a, Num a) => [(a, a)] -> Int
period as = go 1 (stepTime as) where go i xs = if xs == as then i else go (i + 1) $ stepTime xs

energy :: Body -> Int
energy (x, v) = sum (abs x) * sum (abs v)

stepTime :: Num a => [(a, a)] -> [(a, a)]
stepTime = fmap stepPosition . stepVelocities

stepPosition :: Num a => (a, a) -> (a, a)
stepPosition (x, v) = (x + v, v)

stepVelocities :: Num a => [(a, a)] -> [(a, a)]
stepVelocities bs = fmap (\b -> stepVelocity b bs) bs

stepVelocity :: Num a => (a, a) -> [(a, a)] -> (a, a)
stepVelocity = foldr (\(other, _) (x, v) -> (x, v + accelerate other x))

accelerate :: Num a => a -> a -> a
accelerate v w = signum (v - w)

invert :: Body -> Vector (Int, Int)
invert (Vector x y z, Vector u v w) = Vector (x, u) (y, v) (z, w)

moons :: [Body]
moons = initBody <$> [(-16, 15, -9), (-14, 5, 4), (2, 0, 6), (-3, 18, 9)]

initBody :: (Int, Int, Int) -> Body
initBody (x, y, z) = (Vector x y z, Vector 0 0 0)

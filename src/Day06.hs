module Day06 where

import Paths_Advent_of_Code_2k19
import Common.Parsers
import Data.List                      (stripPrefix, tails, inits)
import Data.Map.Strict as M
import Data.Tuple                     ( swap )
import Text.Parsec                    ( parse )
import Text.Parsec.String             ( Parser, parseFromFile )
import Text.Parsec.Char               ( noneOf, char, endOfLine )
import Text.Parsec.Combinator         ( many1, sepBy1 )

type Object = String

type Edge = (Object, Object)

type OrbitMap = Map Object Object

solution1 :: IO Int
solution1 = numOrbits . fromEdges <$> readEdges

solution2 :: IO Int
solution2 = do
    orbitMap <- fromEdges <$> readEdges
    return $ (length $ shortestPath orbitMap "YOU" "SAN") - 1

shortestPath :: OrbitMap -> Object -> Object -> [Object]
shortestPath m o1 o2 = let
    p1 = reachableFrom m o1
    p2 = reachableFrom m o2
    in
        head [x ++ (init y) | x <- (tail $ inits p1), y <- (tail $ inits p2), last x == last y]

numOrbits :: OrbitMap -> Int
numOrbits m = Prelude.foldr (\k i -> i + indirections m k) 0 $ keys m

indirections :: OrbitMap -> Object -> Int 
indirections m = length . reachableFrom m

reachableFrom :: OrbitMap -> Object -> [Object]
reachableFrom m o = case M.lookup o m of
    Nothing -> []
    Just o' -> (o':reachableFrom m o')

fromEdges :: [Edge] -> OrbitMap
fromEdges es = fromList $ swap <$> es

objectP :: Parser Object
objectP = many1 $ noneOf [')', '\n']

edgeP :: Parser Edge
edgeP = (,) <$> objectP <* char ')' <*> objectP

edgesP :: Parser [Edge]
edgesP = sepBy1 edgeP endOfLine

example :: [Edge]
example = case parse edgesP "" "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN" of
    Left err -> error $ show err
    Right succ -> succ

readEdges :: IO [Edge]
readEdges = do
    f <- getDataFileName "input/Day06/input.txt"
    result <- parseFromFile edgesP f 
    case result of
        Left err   -> error $ show err
        Right succ -> return succ

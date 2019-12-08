module Day06 where

import Paths_Advent_of_Code_2k19
import Control.Monad
import Control.Monad.Reader
import Common.Parsers
import Data.List                      ( stripPrefix, tails, inits )
import Data.Map.Strict as M
import Data.Tuple                     ( swap )
import Text.Parsec                    ( parse )
import Text.Parsec.String             ( Parser, parseFromFile )
import Text.Parsec.Char               ( noneOf, char, endOfLine )
import Text.Parsec.Combinator         ( many1, sepBy1 )

type Object = String

type Edge = (Object, Object)

type OrbitMap = Map Object Object

part1 :: IO Int
part1 = runReader numOrbits . fromEdges <$> readEdges

part2 :: IO Int
part2 = (+) (-1) . length . runReader (shortestPath "YOU" "SAN") . fromEdges <$> readEdges

shortestPath :: Object -> Object -> Reader OrbitMap [Object]
shortestPath o1 o2 = do
    p1 <- reachableFrom o1
    p2 <- reachableFrom o2
    return $ head [x ++ init y | x <- tail $ inits p1, y <- tail $ inits p2, last x == last y]

numOrbits :: Reader OrbitMap Int
numOrbits = foldM (\i k -> (+i) <$> indirections k) 0 =<< asks keys

indirections :: Object -> Reader OrbitMap Int
indirections o = length <$> reachableFrom o

reachableFrom :: Object -> Reader OrbitMap [Object]
reachableFrom o = do
    m <- ask
    case M.lookup o m of
        Nothing -> return []
        Just o' -> (o':) <$> reachableFrom o'

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
    f <- getDataFileName "input/Day06.txt"
    result <- parseFromFile edgesP f 
    case result of
        Left err   -> error $ show err
        Right succ -> return succ
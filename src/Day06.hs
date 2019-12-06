module Day06 where

import Paths_Advent_of_Code_2k19
import Common.Parsers
import Data.Map.Strict as M
import Text.Parsec.String             ( Parser, parseFromFile )
import Text.Parsec.Char               ( noneOf, char, endOfLine )
import Text.Parsec.Combinator         ( many1, sepBy1 )

type Object = String

type Edge = (Object, Object)

type OrbitMap = Map Object Object

solution1 :: IO Int
solution1 = numOrbits . fromEdges <$> readEdges

numOrbits :: OrbitMap -> Int
numOrbits m = Prelude.foldr (\k i -> i + indirections m k) 0 $ keys m

indirections :: OrbitMap -> Object -> Int 
indirections m = length . reachableFrom m

reachableFrom :: OrbitMap -> Object -> [Object]
reachableFrom m o = case M.lookup o m of
    Nothing -> []
    Just o' -> (o':reachableFrom m o')

fromEdges :: [Edge] -> OrbitMap
fromEdges = Prelude.foldr (\(com, sat) m -> insert sat com m) mempty

objectP :: Parser Object
objectP = many1 $ noneOf [')', '\n']

edgeP :: Parser Edge
edgeP = (,) <$> objectP <* char ')' <*> objectP

edgesP :: Parser [Edge]
edgesP = sepBy1 edgeP endOfLine

readEdges :: IO [Edge]
readEdges = do
    f <- getDataFileName "input/Day06/input.txt"
    result <- parseFromFile edgesP f 
    case result of
        Left err   -> error $ show err
        Right succ -> return succ

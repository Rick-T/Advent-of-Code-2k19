{-# LANGUAGE TupleSections #-}

module Day24 where

import Paths_Advent_of_Code_2k19
import Common.GridMovement
import Data.List as L (sortOn)
import Data.Map.Strict as M
import Data.Set as S (Set(..), member, insert)
import Data.Tuple (swap)

data Tile = Bug | Free deriving (Eq, Ord)

type Area = Map Position Tile

type Level = Int

type LevelMap = Map Level Area

type StepLevelFunc = (Level, Position) -> Position -> [(Level, Position)]

part1 :: IO Int
part1 = biodiversity . (! 0) . firstDup . timeline stepLevelFlat . loadLevelMap 0 <$> loadArea "input/Day24.txt"

part2 :: IO Int
part2 = countBugs . fmap (M.delete (2, 2)) . (!! 200) . timeline stepLevelRec . loadLevelMap 100 <$> loadArea "input/Day24.txt"

countBugs :: LevelMap -> Int
countBugs = M.foldr (+) 0 . M.map (size . M.filter (== Bug))

biodiversity :: Area -> Int
biodiversity map =
    let
        powersOf2 = fmap (2 ^) [0 ..]
        tiles     = fmap snd $ sortOn (swap . fst) $ M.toList map
    in sum $ fmap snd $ Prelude.filter (\(t, _) -> t == Bug) $ zip tiles powersOf2

firstDup :: (Ord a) => [a] -> a
firstDup = findDup mempty
  where
    findDup :: (Ord a) => Set a -> [a] -> a
    findDup s (m : ms) = if m `S.member` s then m else findDup (m `S.insert` s) ms

timeline :: StepLevelFunc -> LevelMap -> [LevelMap]
timeline stepLevel = iterate $ step stepLevel

step :: StepLevelFunc -> LevelMap -> LevelMap
step stepLevel map = mapWithKey (\lvl area -> mapWithKey (stepTile stepLevel map lvl) area) map

stepTile :: StepLevelFunc -> LevelMap -> Level -> Position -> Tile -> Tile
stepTile stepLevel map lvl pos tile =
    let
        neighborsPos = neighborPositions stepLevel (lvl, pos)
        numBugs      = length $ Prelude.filter (== Bug) $ fmap (\(l, p) -> findWithDefault emptyArea l map ! p) neighborsPos
    in case tile of
        Bug  -> if numBugs == 1 then Bug else Free
        Free -> if numBugs == 1 || numBugs == 2 then Bug else Free

neighborPositions :: StepLevelFunc -> (Level, Position) -> [(Level, Position)]
neighborPositions stepLevel origin@(_, pos) = concat $ stepLevel origin . move pos <$> directions

stepLevelRec :: StepLevelFunc
stepLevelRec (level, (x, y)) (x', y')
    | x' == -1                     = [(level - 1, (1, 2))]
    | x' == 5                      = [(level - 1, (3, 2))]
    | y' == -1                     = [(level - 1, (2, 1))]
    | y' == 5                      = [(level - 1, (2, 3))]
    | x' == 2 && y' == 2 && x == 1 = (level + 1, ) <$> [ (0, y) | y <- [0 .. 4] ]
    | x' == 2 && y' == 2 && x == 3 = (level + 1, ) <$> [ (4, y) | y <- [0 .. 4] ]
    | x' == 2 && y' == 2 && y == 1 = (level + 1, ) <$> [ (x, 0) | x <- [0 .. 4] ]
    | x' == 2 && y' == 2 && y == 3 = (level + 1, ) <$> [ (x, 4) | x <- [0 .. 4] ]
    | otherwise                    = [(level, (x', y'))]


stepLevelFlat :: StepLevelFunc
stepLevelFlat (level, _) (x', y')
    | x' == -1 || x' == 5 || y' == -1 || y' == 5 = []
    | otherwise = [(level, (x', y'))]

parseTile :: Char -> Tile
parseTile '#' = Bug
parseTile '.' = Free

loadLevelMap :: Int -> Area -> LevelMap
loadLevelMap depth initialArea = M.insert 0 initialArea $ M.fromList $ zip [-depth .. depth] $ repeat emptyArea

loadArea :: String -> IO Area
loadArea filename = fmap parseTile . toGrid . lines <$> (readFile =<< getDataFileName filename)

emptyArea :: Area
emptyArea = toGrid $ replicate 5 $ replicate 5 Free

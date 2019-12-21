module Day20 where

import Paths_Advent_of_Code_2k19
import Common.Dijkstra
import Common.GridMovement
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.List as L (delete)
import Data.Maybe as Maybe (catMaybes, mapMaybe)
import Data.HashMap.Lazy as M
import Data.Hashable

data Tile = Wall | Free | P Portal deriving (Eq, Show)

type Portal = (Char, Char, Location)

data Location = Outer | Inner deriving (Eq, Show)

type Maze = HashMap Position Tile

type LevelPosition = (Position, Int)

type SearchFunction a = Maze -> a -> Direction -> Maybe (a, Int)

part1 :: IO (Maybe (Int, ()))
part1 = do
    maze <- loadMaze "input/Day20.txt"
    return $ findExit maze step (entryPos maze) (exitPos maze)

part2 :: IO (Maybe (Int, ()))
part2 = do
    maze <- loadMaze "input/Day20.txt"
    return $ findExit maze stepLevel (entryPos maze, 0) (exitPos maze, 0)

findExit :: (Eq a, Ord a, Hashable a) => Maze -> SearchFunction a -> a -> a -> Maybe (Int, ())
findExit maze searchFunc start goal =
    let
        acc   = const $ const ()
        edges = neighbors maze searchFunc
    in runDijkstra (searchOne goal) edges acc start ()

neighbors :: Maze -> SearchFunction a -> a -> [(a, Int)]
neighbors maze stepFunc pos = catMaybes $ stepFunc maze pos <$> directions

stepLevel :: SearchFunction LevelPosition
stepLevel maze (pos, level) dir =
    let p = move pos dir
    in
        M.lookup p maze >>= \tile -> case tile of
            Wall     -> Nothing
            Free     -> Just ((p, level), 1)
            P portal -> case portal of
                ('A', 'A', _    ) -> Just ((p, level), 1)
                ('Z', 'Z', _    ) -> Just ((p, level), 1)
                (x  , y  , Inner) -> Just ((head $ L.delete p $ posOf tile maze, level + 1), 2)
                (x  , y  , Outer) -> if level == 0 then Nothing else Just ((head $ L.delete p $ posOf tile maze, level - 1), 2)

step :: SearchFunction Position
step maze pos dir =
    let p = move pos dir
    in
        M.lookup p maze >>= \tile -> case tile of
            Wall     -> Nothing
            Free     -> Just (p, 1)
            P portal -> case portal of
                ('A', 'A', _) -> Just (p, 1)
                ('Z', 'Z', _) -> Just (p, 1)
                _             -> Just (head $ L.delete p $ posOf tile maze, 2)

isWalkable :: Tile -> Bool
isWalkable Free  = True
isWalkable (P _) = True
isWalkable Wall  = False

entryPos :: Maze -> Position
entryPos = head . posOf (P ('A', 'A', Outer))

exitPos :: Maze -> Position
exitPos = head . posOf (P ('Z', 'Z', Outer))

posOf :: Tile -> Maze -> [Position]
posOf Wall          = M.keys . M.filter (== Wall)
posOf Free          = M.keys . M.filter (== Free)
posOf (P (x, y, _)) = M.keys . M.filter (\t -> t == P (x, y, Inner) || t == P (x, y, Outer))

parseMaze :: HashMap Position Char -> Maze
parseMaze map = mapWithKey
    (\pos val -> case val of
        '#' -> Wall
        ' ' -> Wall
        '.' -> maybe Free P $ neighboringPortal map pos
        _   -> Wall
    )
    map

neighboringPortal :: HashMap Position Char -> Position -> Maybe Portal
neighboringPortal map pos = case Maybe.mapMaybe (readPortal map pos) directions of
    []  -> Nothing
    [p] -> Just p

readPortal :: HashMap Position Char -> Position -> Direction -> Maybe Portal
readPortal map pos@(x, y) dir =
    let
        p            = move pos dir
        (xMax, yMax) = maximum $ keys map
        location     = if x <= 2 || y <= 2 || x >= (xMax - 2) || y >= (yMax - 2) then Outer else Inner
    in M.lookup p map >>= \c -> case c of
        '#' -> Nothing
        '.' -> Nothing
        _   -> Just $ case dir of
            North -> (map ! move p dir, c, location)
            West  -> (map ! move p dir, c, location)
            South -> (c, map ! move p dir, location)
            East  -> (c, map ! move p dir, location)

loadMaze :: String -> IO Maze
loadMaze filename = parseMaze . toGridHashable . lines <$> (readFile =<< getDataFileName filename)

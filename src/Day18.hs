module Day18 where

import Paths_Advent_of_Code_2k19
import Common.Dijkstra
import Common.GridMovement
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Hashable
import Data.HashMap.Lazy as M
import Data.HashSet as S

data Tile = Robot | Wall | Free | D Door | K Key deriving (Show, Eq)

newtype Door = Door Char deriving (Show, Eq, Ord)

newtype Key = Key Char deriving (Show, Eq, Ord)

type Maze = HashMap Position Tile

type KeyPaths = HashMap Position (HashMap Key (Int, [Door]))

type Inventory = HashSet Key

instance Hashable Key where
    hashWithSalt i (Key c) = hashWithSalt i c
    hash (Key c) = hash c

part1 :: IO (Maybe ((HashSet Position, Inventory), Int, ()))
part1 = collectKeys =<< loadMaze "input/Day18.txt"

part2 :: IO (Maybe ((HashSet Position, Inventory), Int, ()))
part2 = collectKeys =<< (splitMaze <$> loadMaze "input/Day18.txt")

collectKeys :: Maze -> IO (Maybe ((HashSet Position, Inventory), Int, ()))
collectKeys maze = do
    let keyPaths     = calculateKeyPaths maze
    let keyPositions = findKeys maze
    let numKeys      = M.size keyPositions
    let acc          = const $ const ()
    let edges        = nextKeys maze keyPaths
    return $ runDijkstra (searchUntil (\(_, inv) -> S.size inv == numKeys)) edges acc (S.fromList $ startPos maze, mempty) ()

splitMaze :: Maze -> Maze
splitMaze maze =
    let
        (x, y) = head $ startPos maze
        modification (0, 0) = Wall
        modification (dx, dy)
            | abs dx == abs dy = Robot
            | otherwise        = Wall
    in Prelude.foldr (\(dx, dy) m -> M.insert (x + dx, y + dy) (modification (dx, dy)) m) maze [ (dx, dy) | dx <- [-1 .. 1], dy <- [-1 .. 1] ]

nextKeys :: Maze -> KeyPaths -> (HashSet Position, Inventory) -> [((HashSet Position, Inventory), Int)]
nextKeys maze keyPaths (poss, inv) =
    let
        keyPositions = findKeys maze
        nextKeysForPosition pos =
            Data.Maybe.mapMaybe
                    (\k -> M.lookup pos keyPaths >>= M.lookup k >>= \(steps, doors) ->
                        if inv `canOpen` doors then Just ((S.insert (keyPositions ! k) $ S.delete pos poss, S.insert k inv), steps) else Nothing
                    )
                $ S.toList
                $ S.difference allKeys inv
    in concatMap nextKeysForPosition poss

calculateKeyPaths :: Maze -> KeyPaths
calculateKeyPaths maze =
    let
        starts       = startPos maze
        keyPositions = elems $ findKeys maze
    in M.fromList $ fmap (\p -> (p, calculateKeyPath maze p)) (starts ++ keyPositions)

calculateKeyPath :: Maze -> Position -> HashMap Key (Int, [Door])
calculateKeyPath maze pos =
    let
        keyPositions = elems $ findKeys maze
        throughDoor p inv = case M.lookup p maze of
            Just (D door) -> if door `elem` inv then inv else door : inv
            _             -> inv
        edges a = fmap (\p -> (p, 1)) $ Prelude.filter (\p -> p `M.member` (M.filter (isWalkable allKeys) maze)) $ move a <$> directions
        results = runDijkstra (searchMany keyPositions) edges throughDoor pos []
        updateMap (p, s, ks) m = case M.lookup p maze of
            Just (K key) -> M.insert key (s, ks) m
            _            -> m
        merge _ Nothing       = Nothing
        merge x (Just (y, z)) = Just (x, y, z)
    in Prelude.foldr updateMap mempty $ catMaybes $ zipWith merge keyPositions results

isWalkable :: Inventory -> Tile -> Bool
isWalkable _   Robot    = True
isWalkable _   Free     = True
isWalkable _   (K _)    = True
isWalkable _   Wall     = False
isWalkable inv (D door) = any (`opens` door) inv

canOpen :: Inventory -> [Door] -> Bool
canOpen keys = all (\d -> any (`opens` d) keys)

startPos :: Maze -> [Position]
startPos = M.keys . M.filter (== Robot)

findKeys :: Maze -> HashMap Key Position
findKeys m = M.foldrWithKey (\pos (K k) acc -> M.insert k pos acc) mempty $ M.filter isKey m

opens :: Key -> Door -> Bool
opens (Key k) (Door d) = k == d

isKey :: Tile -> Bool
isKey (K _) = True
isKey _     = False

allKeys :: Inventory
allKeys = S.fromList $ fmap Key ['A' .. 'Z']

parseTile :: Char -> Tile
parseTile '#' = Wall
parseTile '.' = Free
parseTile '@' = Robot
parseTile c
    | isLower c = K $ Key $ toUpper c
    | isUpper c = D $ Door c

loadMaze :: String -> IO Maze
loadMaze filename = M.map parseTile . toGridHashable . lines <$> (readFile =<< getDataFileName filename)

module Day25 where

import Paths_Advent_of_Code_2k19
import Common.GridMovement
import Common.Intcode
import Control.Monad.RWS.Strict
import Control.Monad.State
import Data.Char (chr, ord)
import Data.List (group, sort, intercalate, intersperse)
import Data.HashSet as S (HashSet(..), toList, fromList, delete, insert, member, size)

type Item = String

data Room = Room {name :: String, description :: String, doors :: [Direction], items :: HashSet Item} deriving (Eq, Ord)

data Adventure = Adventure {inventory :: HashSet Item, room :: Room} deriving (Eq, Ord)

data Command = Move Direction | Take Item | Drop Item deriving (Eq, Ord)

type AdventureState = StateT Adventure ComputerStateIO

instance Show Command where
    show (Move North) = "north"
    show (Move South) = "south"
    show (Move East ) = "east"
    show (Move West ) = "west"
    show (Take item ) = "take " ++ item
    show (Drop item ) = "drop " ++ item

part1 :: IO ()
part1 = do
    computer <- loadComputer "input/Day25.txt"
    let (Right initialRoom, c', _) = runRWS getRoom 0 computer
    let computerState = evalStateT (forM toCheckpoint stepM *> forceRoom South) (Adventure mempty initialRoom)
    putStr =<< fst <$> evalRWST computerState 0 c'

part2 :: IO ()
part2 = print "Hooray!"


stepM :: Command -> AdventureState (Either [[String]] ())
stepM (Drop i) = do
    lift $ feedInput (compile $ Drop i)
    inv <- gets inventory
    Right <$> modify (\a -> a { inventory = S.delete i inv })
stepM (Take i) = do
    lift $ feedInput (compile $ Take i)
    inv <- gets inventory
    Right <$> modify (\a -> a { inventory = S.insert i inv })
stepM (Move d) = do
    mRoom <- lift $ feedInput (compile $ Move d) *> getRoom
    case mRoom of
        Left  lastBlock -> return $ Left lastBlock
        Right r         -> Right <$> modify (\a -> a { room = r })

toCheckpoint :: [Command]
toCheckpoint =
    replicate 5 (Move East)
        ++ [Take "dark matter"]
        ++ replicate 3 (Move West)
        ++ [Move North, Take "prime number", Move South]
        ++ [ Move West
           , Take "whirled peas"
           , Move North
           , Take "coin"
           , Move West
           , Move North
           , Move West
           , Take "astrolabe"
           , Move East
           , Move South
           , Move South
           , Take "antenna"
           , Move North
           , Move East
           , Move South
           , Move West
           , Move North
           , Take "fixed point"
           , Move North
           , Take "weather machine"
           , Move East
           ]

combinations :: (Ord a, Eq a) => [a] -> [[a]]
combinations = fmap head . group . sort . combinations'

combinations' :: Eq a => [a] -> [[a]]
combinations' [] = []
combinations' l  = l : concat ([ combinations' $ Prelude.filter (/= a) l | a <- l ])

forceRoom :: Direction -> AdventureState String
forceRoom dir = do
    items <- S.toList <$> gets inventory
    forceRoom' (combinations items) dir

forceRoom' :: [[Item]] -> Direction -> AdventureState String
forceRoom' (comb : combs) dir = do
    inv <- S.toList <$> gets inventory
    forM_ (Prelude.filter (`notElem` comb) inv) $ stepM . Drop
    forM_ (Prelude.filter (`notElem` inv) comb) $ stepM . Take
    result <- stepM (Move dir)
    case result of
        Right _  -> forceRoom' combs dir
        Left  bs -> return $ intercalate "\n\n" $ fmap (intercalate "\n") bs

compile :: Show a => a -> [Int]
compile s = (ord <$> show s) ++ [ord '\n']

decompile :: [Int] -> String
decompile is = init $ chr <$> is

compileS :: String -> [Int]
compileS s = (ord <$> s) ++ [ord '\n']

gameLoop :: ComputerStateT IO ()
gameLoop = do
    output <- readLine
    case output of
        Nothing -> return ()
        Just s  -> do
            liftIO $ putStrLn s
            when (s == "Command?") $ feedInput =<< (compileS <$> liftIO getLine)
            gameLoop

getRoom :: Monad m => ComputerStateT m (Either [[String]] Room)
getRoom = do
    bs <- readBlocks
    if last bs /= ["Command?"]
        then return $ Left bs
        else
            let
                blocks      = if length bs <= 4 then bs else dropWhile (not . isRoomBlock) $ drop (length bs - 5) bs
                name        = blocks !! 0 !! 0
                description = blocks !! 0 !! 1
                directions  = parseDirection <$> (tail $ blocks !! 1)
                items       = if blocks !! 2 == ["Command?"] then mempty else S.fromList $ parseItem <$> (tail $ blocks !! 2)
            in return $ Right $ Room name description directions items

isRoomBlock :: [String] -> Bool
isRoomBlock (('=' : '=' : ' ' : _) : as : _) = True
isRoomBlock _ = False

parseDirection :: String -> Direction
parseDirection "- north" = North
parseDirection "- east"  = East
parseDirection "- west"  = West
parseDirection "- south" = South

parseItem :: String -> Item
parseItem = drop 2

readBlocks :: Monad m => ComputerStateT m [[String]]
readBlocks = do
    (hasMore, block) <- readBlock
    if hasMore then (block :) <$> readBlocks else return [block]

readBlock :: Monad m => ComputerStateT m (Bool, [String])
readBlock = do
    output <- readLine
    case output of
        Nothing         -> return (False, [])
        Just "Command?" -> return (False, ["Command?"])
        Just ""         -> return (True, [])
        Just s          -> fmap (s :) <$> readBlock

readLine :: Monad m => ComputerStateT m (Maybe String)
readLine = do
    output <- nextOutput
    case output of
        Nothing -> return Nothing
        Just 10 -> return $ Just ""
        Just i  -> fmap (chr i :) <$> readLine

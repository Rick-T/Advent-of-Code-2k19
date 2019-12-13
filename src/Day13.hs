module Day13 (part1, part2) where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Data.Map.Strict as M
import System.Console.ANSI
import System.IO
import System.Timeout

data Tile = Empty | Wall | Block | Paddle | Ball

type Position = (Val, Val)

data Breakout = Breakout { paddlePos :: Maybe Position, ballPos :: Maybe Position, score :: Val, screen :: Map Position Tile }

type BreakoutState = StateT Breakout ComputerStateIO

type InputMethod = BreakoutState Val

type OutputMethod = Position -> BreakoutState ()

part1 :: IO Int
part1 = fmap fst $ evalRWST (evalStateT (runGame noInput noOutput *> countBlocks) initBreakout) 0 =<< loadComputer "input/Day13.txt"

part2 :: IO Val
part2 = hideCursor *> hSetBuffering stdin NoBuffering *> hSetEcho stdin False *> clearScreen *> fmap
  fst
  (evalRWST (evalStateT ((lift $ writeAddr 0 2) *> (runGame interactive printScreen) *> gets score) initBreakout) 0 =<< loadComputer "input/Day13.txt")

runGame :: InputMethod -> OutputMethod -> BreakoutState ()
runGame getInput showOutput = do
  input   <- getInput
  outputs <- lift $ withInput input $ nextOutputs 3
  case outputs of
    (-1 : 0 : s : _) -> do
      modify $ \m -> m { score = s }
      showOutput (-1, 0)
      runGame getInput showOutput
    (x : y : tile : _) -> do
      s <- gets screen
      case toTile tile of
        Ball   -> modify $ \m -> m { screen = insert (x, y) Ball s, ballPos = Just (x, y) }
        Paddle -> modify $ \m -> m { screen = insert (x, y) Paddle s, paddlePos = Just (x, y) }
        t      -> modify $ \m -> m { screen = insert (x, y) t s }
      showOutput (x, y)
      runGame getInput showOutput
    _ -> return ()

countBlocks :: BreakoutState Int
countBlocks = gets $ length . elems . M.filter isBlock . screen

noOutput :: OutputMethod
noOutput _ = return ()

printScore :: OutputMethod
printScore (-1, 0) = do
  s <- gets score
  liftIO $ setCursorPosition 0 0
  liftIO $ clearLine
  liftIO $ putStrLn $ "Score " ++ show s
printScore p = noOutput p

printScreen :: OutputMethod
printScreen (-1, 0) = printScore (-1, 0)
printScreen (x , y) = do
  screen <- gets screen
  liftIO $ setCursorPosition (fromInteger y + 1) $ fromInteger x
  liftIO $ putChar $ toReadable $ screen ! (x, y)

noInput :: InputMethod
noInput = return 0

joystickAI :: InputMethod
joystickAI = do
  p <- gets paddlePos
  b <- gets ballPos
  case (p, b) of
    (Nothing, _      ) -> return 0
    (_      , Nothing) -> return 0
    (Just p , Just b ) -> return $ signum $ fst b - fst p

interactive :: InputMethod
interactive = do
  mChar <- liftIO $ timeout 30000 getChar
  case mChar of
    (Just 'a') -> return (-1)
    (Just 'd') -> return 1
    _          -> return 0

isPaddle :: Tile -> Bool
isPaddle Paddle = True
isPaddle _      = False

isBall :: Tile -> Bool
isBall Ball = True
isBall _    = False

isBlock :: Tile -> Bool
isBlock Block = True
isBlock _     = False

toTile :: Val -> Tile
toTile 0 = Empty
toTile 1 = Wall
toTile 2 = Block
toTile 3 = Paddle
toTile 4 = Ball

toReadable :: Tile -> Char
toReadable Empty  = ' '
toReadable Wall   = '\x2588'
toReadable Block  = '\x2592'
toReadable Paddle = '\x2580'
toReadable Ball   = '\x2B24'

initBreakout :: Breakout
initBreakout = Breakout Nothing Nothing 0 mempty

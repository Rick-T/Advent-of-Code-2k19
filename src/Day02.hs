module Day02 where

import Paths_Advent_of_Code_2k19
import Common.Parsers
import Control.Monad (unless)
import Control.Monad.State
import Data.Either
import Data.Sequence as S
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (oneOf, char, digit)
import Text.Parsec.Combinator (many1, sepBy1)

type Val = Int

type Memory = Seq Val

type Addr = Int

type InstructionPointer = Addr

data Computer = Computer { inst :: InstructionPointer, mem :: Memory } deriving Show

type ComputerState = State Computer

data OpCodeAction = Add | Mult

data OpCode = Act OpCodeAction | Term

type Done = Bool

toOpCode :: Val -> OpCode
toOpCode 1 = Act Add
toOpCode 2 = Act Mult
toOpCode 99 = Term
toOpCode _ = error "Invalid opcode"

solution1 :: IO Int
solution1 = evalState evaluateProgram . initMemory 12 2 <$> loadMemory

solution2 :: IO Int
solution2 = (\m -> head [ 100 * x + y | x <- [0 .. 99], y <- [0 .. 99], evalState evaluateProgram (initMemory x y m) == 19690720]) <$> loadMemory

evaluateProgram :: ComputerState Int
evaluateProgram = runProgram *> gets (flip index 0 . mem)

runProgram :: ComputerState ()
runProgram = do
  done <- stepOnce
  unless done runProgram

stepOnce :: ComputerState Done
stepOnce = do
  curVal <- consumeInst
  case toOpCode curVal of
    Term  -> return True
    Act a -> executeAction a >>= (\() -> return False)

executeAction :: OpCodeAction -> ComputerState ()
executeAction Add = arithmeticAction (+)
executeAction Mult = arithmeticAction (*)

arithmeticAction :: (Val -> Val -> Val) -> ComputerState ()
arithmeticAction f = do
  xp <- consumeInst
  yp <- consumeInst
  zp <- consumeInst
  x <- readAddr xp
  y <- readAddr yp
  writeAddr zp (f x y)

consumeInst :: ComputerState Val
consumeInst = do
  x <- readInst
  incrementInst
  return x

incrementInst :: ComputerState ()
incrementInst = modify $ \(Computer inst mem) -> Computer (inst + 1) mem

readInst :: ComputerState Val
readInst = do
  (Computer inst _) <- get
  readAddr inst

readAddr :: Addr -> ComputerState Val
readAddr x = do
  (Computer _ mem) <- get
  case S.lookup x mem of
    Just val -> return val
    Nothing  -> error "Sequence index out of bounds"

writeAddr :: Addr -> Val -> ComputerState ()
writeAddr addr val = modify $ \(Computer inst mem) -> Computer inst (update addr val mem)

testInit :: IO Computer
testInit = initMemory 12 2 <$> loadMemory

initMemory :: Int -> Int -> Memory -> Computer
initMemory noun verb mem = let 
  mem' = update 1 noun mem
  mem'' = update 2 verb mem'
  in 
    Computer 0 mem''

memory :: Parser Memory
memory = do
  mem <- num `sepBy1` (char ',')
  return $ fromList mem

loadMemory :: IO Memory
loadMemory = do
  fileName <- getDataFileName "input/Day02/input01.txt"
  result <- parseFromFile memory fileName
  case result of
    Left err   -> error $ show err
    Right succ -> return succ
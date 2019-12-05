module Intcode where

import Paths_Advent_of_Code_2k19
import Common.Parsers (num)
import Control.Monad (unless, when)
import Control.Monad.Trans.RWS.Lazy
import Data.Sequence as S hiding (length, reverse, take, drop)
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy1)

type Memory = Seq Int

type Addr = Int

type InstructionPointer = Addr

data Computer = Computer { inst :: InstructionPointer, mem :: Memory } deriving Show

type ComputerState = RWS Int [Int] Computer

data ParameterMode = Position | Immediate deriving Show

data OpCode = Add (ParameterMode, ParameterMode) | Mult (ParameterMode, ParameterMode) | Input | Output ParameterMode | JumpT (ParameterMode, ParameterMode) | JumpF (ParameterMode, ParameterMode) | IsLess (ParameterMode, ParameterMode) | IsEqual (ParameterMode, ParameterMode) | Term deriving Show

type Done = Bool

solution1 :: IO Int
solution1 = fst . evalRWS (runProgram *> readAddr 0) 1 <$> initComputer <$> initMemory 12 2 <$> loadMemory "input/Day02/input01.txt"

solution2 :: IO Int
solution2 = (\m -> head [ 100 * x + y | x <- [0 .. 99], y <- [0 .. 99], (fst . evalRWS (runProgram *> readAddr 0) 1 $ initComputer $ initMemory x y m) == 19690720]) <$> loadMemory "input/Day02/input01.txt"

solution3 :: IO [Int]
solution3 = snd . evalRWS runProgram 1 <$> initComputer <$> loadMemory "input/Day05/input01.txt"

solution4 :: IO [Int]
solution4 = snd . evalRWS runProgram 5 <$> initComputer <$> loadMemory "input/Day05/input01.txt"

checkExample :: Int -> IO [Int]
checkExample i = snd . evalRWS runProgram i <$> initComputer <$> loadMemory "input/Day05/example.txt"

runProgram :: ComputerState ()
runProgram = do
  done <- stepOnce
  unless done runProgram

stepOnce :: ComputerState Done
stepOnce = do
  curVal <- consumeInst
  case toOpCode curVal of
    Term  -> return True
    a -> executeAction a >>= (\() -> return False)

executeAction :: OpCode -> ComputerState ()
executeAction (Add ms) = arithmeticAction ms (+)
executeAction (Mult ms) = arithmeticAction ms (*)
executeAction Input = processInput
executeAction (Output m) = processOutput m
executeAction (JumpT ms) = jumpIf ms (/= 0)
executeAction (JumpF ms) = jumpIf ms (== 0)
executeAction (IsLess ms) = Intcode.compare ms (<)
executeAction (IsEqual ms) = Intcode.compare ms (==)

arithmeticAction :: (ParameterMode, ParameterMode) -> (Int -> Int -> Int) -> ComputerState ()
arithmeticAction (m1, m2) f = do
  x <- consumeParam m1
  y <- consumeParam m2
  zp <- consumeInst
  writeAddr zp (f x y)

processInput :: ComputerState ()
processInput = do
  p <- consumeInst
  i <- ask
  writeAddr p i

processOutput :: ParameterMode -> ComputerState ()
processOutput m = do
  x <- consumeParam m
  tell [x]

jumpIf :: (ParameterMode, ParameterMode) -> (Int -> Bool) -> ComputerState ()
jumpIf (m1, m2) pred = do
  x <- consumeParam m1
  p <- consumeParam m2
  when (pred x) $ modify $ \(Computer _ mem) -> Computer p mem

compare :: (ParameterMode, ParameterMode) -> (Int -> Int -> Bool) -> ComputerState ()
compare (m1, m2) pred = do
  x <- consumeParam m1
  y <- consumeParam m2
  p <- consumeInst
  if pred x y then writeAddr p 1 else writeAddr p 0

consumeParam :: ParameterMode -> ComputerState Int
consumeParam m = do
  p <- consumeInst
  readParam m p

consumeInst :: ComputerState Int
consumeInst = do
  x <- readInst
  incrementInst
  return x

readParam :: ParameterMode -> Addr -> ComputerState Int
readParam m x = case m of
  Position -> readAddr x
  Immediate -> return x

readAddr :: Addr -> ComputerState Int
readAddr x = do
  (Computer _ mem) <- get
  case S.lookup x mem of
    Just val -> return val
    Nothing  -> error "Sequence index out of bounds"

readInst :: ComputerState Int
readInst = do
  (Computer inst _) <- get
  readAddr inst

writeAddr :: Addr -> Int -> ComputerState ()
writeAddr addr val = modify $ \(Computer inst mem) -> Computer inst (update addr val mem)

incrementInst :: ComputerState ()
incrementInst = modify $ \(Computer inst mem) -> Computer (inst + 1) mem

toOpCode :: Int -> OpCode
toOpCode val = let
  code = val `mod` 100
  (m1:m2:_) = (reverse . fmap toParameterMode . toDigits $ val `div` 100) ++ repeat Position
  in 
    case code of
      1 -> Add (m1, m2)
      2 -> Mult (m1, m2)
      3 -> Input
      4 -> Output m1
      5 -> JumpT (m1, m2)
      6 -> JumpF (m1, m2)
      7 -> IsLess (m1, m2)
      8 -> IsEqual (m1, m2)
      99 -> Term

toParameterMode :: Int -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate

toDigits :: Int -> [Int]
toDigits = fmap (read . return) <$> show

toNumber :: [Int] -> Int
toNumber = foldl (\total p -> 10 * total + p) 0

initComputer :: Memory -> Computer
initComputer = Computer 0 

initMemory :: Int -> Int -> Memory -> Memory
initMemory noun verb mem = let 
  mem' = update 1 noun mem
  in 
    update 2 verb mem'

memory :: Parser Memory
memory = do
  mem <- num `sepBy1` char ','
  return $ fromList mem

loadMemory :: FilePath -> IO Memory
loadMemory f = do
  fileName <- getDataFileName f
  result <- parseFromFile memory fileName
  case result of
    Left err   -> error $ show err
    Right succ -> return succ
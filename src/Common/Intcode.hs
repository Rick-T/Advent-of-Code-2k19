module Common.Intcode where

import Paths_Advent_of_Code_2k19
import Common.Parsers ( num )
import Control.Monad ( when )
import Control.Monad.Trans.RWS.Lazy
import Data.Sequence as S hiding ( reverse )
import Text.Parsec.String ( Parser, parseFromFile )
import Text.Parsec.Char ( char )
import Text.Parsec.Combinator ( sepBy1 )

type Memory = Seq Int

type Addr = Int

type InstructionPointer = Addr

data Computer = Computer {inst :: InstructionPointer, mem :: Memory } deriving Show

type ComputerState = RWS Int [Int] Computer

data ParameterMode = Position | Immediate deriving Show

data OpCode = Add (ParameterMode, ParameterMode) | Mult (ParameterMode, ParameterMode) | Input | Output ParameterMode | JumpT (ParameterMode, ParameterMode) | JumpF (ParameterMode, ParameterMode) | IsLess (ParameterMode, ParameterMode) | IsEqual (ParameterMode, ParameterMode) | Term deriving Show

data StepResult = Done TerminationReason | NotDone

data TerminationReason = Terminated | ConditionReached

runProgram :: ComputerState ()
runProgram = runUntil isTerm *> return ()  

runUntil :: (OpCode -> Bool) -> ComputerState TerminationReason
runUntil stopCode = do
  stepResult <- stepOnce stopCode
  case stepResult of
    NotDone -> runUntil stopCode
    Done t -> return t

stepOnce :: (OpCode -> Bool) -> ComputerState StepResult
stepOnce stopPred = do
  curVal <- readInst
  case toOpCode curVal of
    Term -> return $ Done Terminated
    a -> incrementInst *> executeAction a *> (return $ if stopPred a then Done ConditionReached else NotDone)

isTerm :: OpCode -> Bool
isTerm Term = True
isTerm _ = False

isInput :: OpCode -> Bool
isInput Input = True
isInput _ = False

isOutput :: OpCode -> Bool
isOutput (Output _) = True
isOutput _ = False

executeAction :: OpCode -> ComputerState ()
executeAction (Add ms) = arithmeticAction ms (+)
executeAction (Mult ms) = arithmeticAction ms (*)
executeAction Input = processInput
executeAction (Output m) = processOutput m
executeAction (JumpT ms) = jumpIf ms (/= 0)
executeAction (JumpF ms) = jumpIf ms (== 0)
executeAction (IsLess ms) = Common.Intcode.compare ms (<)
executeAction (IsEqual ms) = Common.Intcode.compare ms (==)
executeAction Term = return ()

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
readInst = readAddr =<< inst <$> get

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

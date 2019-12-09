module Common.Intcode where

import Paths_Advent_of_Code_2k19
import Common.Parsers ( num )
import Control.Monad ( when )
import Control.Monad.Trans.RWS.Lazy
import Common.Util ( toDigits )
import Data.Functor ( ($>) )
import Data.Map.Strict as M
import Text.Parsec.String ( Parser, parseFromFile )
import Text.Parsec.Char ( char )
import Text.Parsec.Combinator ( sepBy1 )

type Memory = Map Integer Integer

type Addr = Integer

type InstructionPointer = Addr

data Computer = Computer {inst :: InstructionPointer, offset :: Integer, mem :: Memory } deriving Show

type ComputerState = RWS Integer [Integer] Computer

data ParameterMode = Position | Immediate | Relative deriving Show

data OpCode = Add (ParameterMode, ParameterMode, ParameterMode) | Mult (ParameterMode, ParameterMode, ParameterMode) | Input ParameterMode | Output ParameterMode | JumpT (ParameterMode, ParameterMode) | JumpF (ParameterMode, ParameterMode) | IsLess (ParameterMode, ParameterMode, ParameterMode) | IsEqual (ParameterMode, ParameterMode, ParameterMode) | Offset ParameterMode | Term deriving Show

data StepResult = Done TerminationReason | NotDone

data TerminationReason = Terminated | ConditionReached

runProgram :: ComputerState ()
runProgram = runUntil isTerm $> ()  

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
    a -> incrementInst *> executeAction a $> if stopPred a then Done ConditionReached else NotDone

isTerm :: OpCode -> Bool
isTerm Term = True
isTerm _ = False

isInput :: OpCode -> Bool
isInput (Input _) = True
isInput _ = False

isOutput :: OpCode -> Bool
isOutput (Output _) = True
isOutput _ = False

executeAction :: OpCode -> ComputerState ()
executeAction (Add ms) = arithmeticAction ms (+)
executeAction (Mult ms) = arithmeticAction ms (*)
executeAction (Input m) = processInput m
executeAction (Output m) = processOutput m
executeAction (JumpT ms) = jumpIf ms (/= 0)
executeAction (JumpF ms) = jumpIf ms (== 0)
executeAction (IsLess ms) = Common.Intcode.compare ms (<)
executeAction (IsEqual ms) = Common.Intcode.compare ms (==)
executeAction (Offset m) = modifyOffset m
executeAction Term = return ()

arithmeticAction :: (ParameterMode, ParameterMode, ParameterMode) -> (Integer -> Integer -> Integer) -> ComputerState ()
arithmeticAction (m1, m2, m3) f = do
  x <- consumeParam m1
  y <- consumeParam m2
  zp <- consumeAddr m3
  writeAddr zp (f x y)

processInput :: ParameterMode -> ComputerState ()
processInput m = do
  p <- consumeAddr m
  i <- ask
  writeAddr p i

processOutput :: ParameterMode -> ComputerState ()
processOutput m = do
  x <- consumeParam m
  tell [x]

jumpIf :: (ParameterMode, ParameterMode) -> (Integer -> Bool) -> ComputerState ()
jumpIf (m1, m2) pred = do
  x <- consumeParam m1
  p <- consumeParam m2
  when (pred x) $ modify $ \c -> c { inst = p }

compare :: (ParameterMode, ParameterMode, ParameterMode) -> (Integer -> Integer -> Bool) -> ComputerState ()
compare (m1, m2, m3) pred = do
  x <- consumeParam m1
  y <- consumeParam m2
  p <- consumeAddr m3
  if pred x y then writeAddr p 1 else writeAddr p 0

modifyOffset :: ParameterMode -> ComputerState ()
modifyOffset m = do
  change <- consumeParam m
  off <- gets offset
  modify (\a -> a { offset = off + change })

consumeParam :: ParameterMode -> ComputerState Integer
consumeParam m = do
  p <- consumeInst
  readParam m p

consumeAddr :: ParameterMode -> ComputerState Integer
consumeAddr m = do
  o <- gets offset
  i <- consumeInst
  return $ case m of
    Position -> i
    Relative -> i + o
    Immediate -> error "Immediate read not supported"

consumeInst :: ComputerState Integer
consumeInst = do
  x <- readInst
  incrementInst
  return x

readParam :: ParameterMode -> Addr -> ComputerState Integer
readParam m x = do
  offset <- gets offset
  case m of
    Position -> readAddr x
    Immediate -> return x
    Relative -> readAddr (x + offset)

readAddr :: Addr -> ComputerState Integer
readAddr x = do
  mem <- gets mem
  case M.lookup x mem of
    Just val -> return val
    Nothing  -> return 0

readInst :: ComputerState Integer
readInst = readAddr =<< inst <$> get

writeAddr :: Addr -> Integer -> ComputerState ()
writeAddr addr val = modify $ \(Computer inst offset mem) -> Computer inst offset (insert addr val mem)

incrementInst :: ComputerState ()
incrementInst = modify $ \(Computer inst offset mem) -> Computer (inst + 1) offset mem

toOpCode :: Integer -> OpCode
toOpCode val = let
  code = val `mod` 100
  (m1:m2:m3:_) = (reverse . fmap toParameterMode . toDigits $ val `div` 100) ++ repeat Position
  in 
    case code of
      1 -> Add (m1, m2, m3)
      2 -> Mult (m1, m2, m3)
      3 -> Input m1
      4 -> Output m1
      5 -> JumpT (m1, m2)
      6 -> JumpF (m1, m2)
      7 -> IsLess (m1, m2, m3)
      8 -> IsEqual (m1, m2, m3)
      9 -> Offset m1
      99 -> Term
      c -> error $ "Invalid OpCode: " ++ show c

toParameterMode :: Integer -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate
toParameterMode 2 = Relative
toParameterMode m = error $ "Invalid parameter mode: " ++ show m

initComputer :: Memory -> Computer
initComputer = Computer 0 0

initMemory :: Integer -> Integer -> Memory -> Memory
initMemory noun verb mem = let 
  mem' = insert 1 noun mem
  in 
    insert 2 verb mem'

memory :: Parser Memory
memory = do
  mem <- num `sepBy1` char ','
  return $ fromList $ zip [0..] mem

loadMemory :: FilePath -> IO Memory
loadMemory f = do
  fileName <- getDataFileName f
  result <- parseFromFile memory fileName
  case result of
    Left err   -> error $ show err
    Right succ -> return succ

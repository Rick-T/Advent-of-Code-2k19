module Common.Intcode where

import Paths_Advent_of_Code_2k19
import Common.Parsers (num)
import Control.Monad (liftM2, when)
import Control.Monad.RWS.Lazy
import Common.Util (toDigits)
import Data.Functor (($>))
import Data.Map.Strict as M
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy1)

type Val = Integer

type Memory = Map Val Val

type Addr = Val

type InstructionPointer = Addr

data Computer = Computer {inst :: InstructionPointer, offset :: Val, mem :: Memory } deriving Show

type ComputerState = RWS Val [Val] Computer

data ParameterMode = Position | Immediate | Relative deriving Show

data OpCode = Add (ParameterMode, ParameterMode, ParameterMode)
            | Mult (ParameterMode, ParameterMode, ParameterMode)
            | Input ParameterMode | Output ParameterMode
            | JumpT (ParameterMode, ParameterMode)
            | JumpF (ParameterMode, ParameterMode)
            | IsLess (ParameterMode, ParameterMode, ParameterMode)
            | IsEqual (ParameterMode, ParameterMode, ParameterMode)
            | Offset ParameterMode
            | Term
            deriving Show

data StepResult = Done TerminationReason | NotDone

data TerminationReason = Terminated | ConditionReached

runProgram :: ComputerState ()
runProgram = runUntil isTerm $> ()

runUntil :: (OpCode -> Bool) -> ComputerState TerminationReason
runUntil stopCode = do
  stepResult <- stepOnce stopCode
  case stepResult of
    NotDone -> runUntil stopCode
    Done t  -> return t

runUntilNext :: (OpCode -> Bool) -> ComputerState TerminationReason
runUntilNext stopCode = do
  next <- toOpCode <$> readInst
  case next of
    Term -> return Terminated
    a
      | stopCode a -> return ConditionReached
      | otherwise  -> stepOnce stopCode *> runUntilNext stopCode

nextOutput :: ComputerState (Maybe Val)
nextOutput = do
  terminationReason <- runUntilNext isOutput
  case terminationReason of
    Terminated       -> return Nothing
    ConditionReached -> Just <$> (toOpCode <$> consumeInst >>= processOutput')

nextOutputs :: Int -> ComputerState [Val]
nextOutputs 0 = return []
nextOutputs i = do
  o <- nextOutput
  case o of
    Nothing -> return []
    Just o' -> do
      others <- nextOutputs (i - 1)
      return $ o' : others

stepOnce :: (OpCode -> Bool) -> ComputerState StepResult
stepOnce stopPred = do
  curVal <- readInst
  case toOpCode curVal of
    Term -> return $ Done Terminated
    a    -> incrementInst *> executeAction a $> if stopPred a then Done ConditionReached else NotDone

withInput :: Val -> ComputerState a -> ComputerState a
withInput i = local (const i)

isTerm :: OpCode -> Bool
isTerm Term = True
isTerm _    = False

isInput :: OpCode -> Bool
isInput (Input _) = True
isInput _         = False

isOutput :: OpCode -> Bool
isOutput (Output _) = True
isOutput _          = False

executeAction :: OpCode -> ComputerState ()
executeAction (Add     ms) = arithmeticAction ms (+)
executeAction (Mult    ms) = arithmeticAction ms (*)
executeAction (Input   m ) = processInput m
executeAction (Output  m ) = processOutput m $> ()
executeAction (JumpT   ms) = jumpIf ms (/= 0)
executeAction (JumpF   ms) = jumpIf ms (== 0)
executeAction (IsLess  ms) = Common.Intcode.compare ms (<)
executeAction (IsEqual ms) = Common.Intcode.compare ms (==)
executeAction (Offset  m ) = modifyOffset m
executeAction Term         = return ()

arithmeticAction :: (ParameterMode, ParameterMode, ParameterMode) -> (Val -> Val -> Val) -> ComputerState ()
arithmeticAction (m1, m2, m3) f = liftM2 f (consumeParam m1) (consumeParam m2) >>= consumeWrite m3

processInput :: ParameterMode -> ComputerState ()
processInput m = ask >>= consumeWrite m

processOutput :: ParameterMode -> ComputerState Val
processOutput m = do
  x <- consumeParam m
  tell [x]
  return x

processOutput' :: OpCode -> ComputerState Val
processOutput' (Output m) = processOutput m

jumpIf :: (ParameterMode, ParameterMode) -> (Val -> Bool) -> ComputerState ()
jumpIf (m1, m2) pred = do
  x <- consumeParam m1
  p <- consumeParam m2
  when (pred x) $ modify $ \c -> c { inst = p }

compare :: (ParameterMode, ParameterMode, ParameterMode) -> (Val -> Val -> Bool) -> ComputerState ()
compare (m1, m2, m3) pred = do
  x <- consumeParam m1
  y <- consumeParam m2
  consumeWrite m3 $ if pred x y then 1 else 0

modifyOffset :: ParameterMode -> ComputerState ()
modifyOffset m = do
  change <- consumeParam m
  off    <- gets offset
  modify (\c -> c { offset = off + change })

consumeParam :: ParameterMode -> ComputerState Val
consumeParam m = consumeInst >>= readParam m

consumeWrite :: ParameterMode -> Val -> ComputerState ()
consumeWrite m v = do
  o <- gets offset
  i <- consumeInst
  case m of
    Position  -> writeAddr i v
    Relative  -> writeAddr (i + o) v
    Immediate -> error "Immediate write not supported"

consumeInst :: ComputerState Val
consumeInst = readInst <* incrementInst

readParam :: ParameterMode -> Addr -> ComputerState Val
readParam m x = do
  offset <- gets offset
  case m of
    Position  -> readAddr x
    Immediate -> return x
    Relative  -> readAddr (x + offset)

readAddr :: Addr -> ComputerState Val
readAddr x = do
  mem <- gets mem
  case M.lookup x mem of
    Just val -> return val
    Nothing  -> return 0

readInst :: ComputerState Val
readInst = readAddr =<< inst <$> get

writeAddr :: Addr -> Val -> ComputerState ()
writeAddr addr val = do
  mem <- gets mem
  modify $ \c -> c { mem = insert addr val mem }

incrementInst :: ComputerState ()
incrementInst = do
  inst <- gets inst
  modify $ \c -> c { inst = (inst + 1) }

toOpCode :: Val -> OpCode
toOpCode val =
  let
    code               = val `mod` 100
    (m1 : m2 : m3 : _) = (reverse . fmap toParameterMode . toDigits $ val `div` 100) ++ repeat Position
  in case code of
    1  -> Add (m1, m2, m3)
    2  -> Mult (m1, m2, m3)
    3  -> Input m1
    4  -> Output m1
    5  -> JumpT (m1, m2)
    6  -> JumpF (m1, m2)
    7  -> IsLess (m1, m2, m3)
    8  -> IsEqual (m1, m2, m3)
    9  -> Offset m1
    99 -> Term
    c  -> error $ "Invalid OpCode: " ++ show c

toParameterMode :: Val -> ParameterMode
toParameterMode 0 = Position
toParameterMode 1 = Immediate
toParameterMode 2 = Relative
toParameterMode m = error $ "Invalid parameter mode: " ++ show m

initComputer :: Memory -> Computer
initComputer = Computer 0 0

memory :: Parser Memory
memory = fromList . zip [0 ..] <$> num `sepBy1` char ','

loadComputer :: FilePath -> IO Computer
loadComputer f = initComputer <$> loadMemory f

loadMemory :: FilePath -> IO Memory
loadMemory f = do
  fileName <- getDataFileName f
  result   <- parseFromFile memory fileName
  case result of
    Left  err  -> error $ show err
    Right succ -> return succ

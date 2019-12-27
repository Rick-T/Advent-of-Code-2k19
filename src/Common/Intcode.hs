module Common.Intcode where

import Paths_Advent_of_Code_2k19
import Common.Parsers (num)
import Control.Monad (liftM2, when)
import Control.Monad.RWS.Strict
import Common.Util (toDigits)
import Data.Functor (($>))
import Data.Functor.Identity
import Data.HashMap.Strict as M
import Text.Parsec.String (Parser, parseFromFile)
import Text.Parsec.Char (char)
import Text.Parsec.Combinator (sepBy1)

type Val = Int

type Memory = HashMap Val Val

type Addr = Val

type InstructionPointer = Addr

data Computer = Computer {inst :: InstructionPointer, offset :: Val, mem :: Memory } deriving (Eq, Ord, Show)

type ComputerState = ComputerStateT Identity

type ComputerStateIO = RWST Val [Val] Computer IO

type ComputerStateT = RWST Val [Val] Computer

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

runProgram :: Monad a => ComputerStateT a ()
runProgram = runUntil isTerm $> ()

runUntil :: Monad a => (OpCode -> Bool) -> ComputerStateT a TerminationReason
runUntil stopCode = do
  stepResult <- stepOnce stopCode
  case stepResult of
    NotDone -> runUntil stopCode
    Done t  -> return t

runUntilNext :: Monad a => (OpCode -> Bool) -> ComputerStateT a TerminationReason
runUntilNext stopCode = do
  next <- toOpCode <$> readInst
  case next of
    Term -> return Terminated
    a
      | stopCode a -> return ConditionReached
      | otherwise  -> stepOnce stopCode *> runUntilNext stopCode

nextOutput :: Monad a => ComputerStateT a (Maybe Val)
nextOutput = do
  terminationReason <- runUntilNext isOutput
  case terminationReason of
    Terminated       -> return Nothing
    ConditionReached -> Just <$> (toOpCode <$> consumeInst >>= processOutput')

nextOutputs :: Monad a => Int -> ComputerStateT a [Val]
nextOutputs 0 = return []
nextOutputs i = do
  o <- nextOutput
  case o of
    Nothing -> return []
    Just o' -> do
      others <- nextOutputs (i - 1)
      return $ o' : others

feedInput :: Monad a => [Val] -> ComputerStateT a ()
feedInput []       = return ()
feedInput (a : as) = withInput a (runUntil isInput) *> feedInput as

stepOnce :: Monad a => (OpCode -> Bool) -> ComputerStateT a StepResult
stepOnce stopPred = do
  curVal <- readInst
  case toOpCode curVal of
    Term -> return $ Done Terminated
    a    -> incrementInst *> executeAction a $> if stopPred a then Done ConditionReached else NotDone

withInput :: Monad a => Val -> ComputerStateT a b -> ComputerStateT a b
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

executeAction :: Monad a => OpCode -> ComputerStateT a ()
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

arithmeticAction :: Monad a => (ParameterMode, ParameterMode, ParameterMode) -> (Val -> Val -> Val) -> ComputerStateT a ()
arithmeticAction (m1, m2, m3) f = liftM2 f (consumeParam m1) (consumeParam m2) >>= consumeWrite m3

processInput :: Monad a => ParameterMode -> ComputerStateT a ()
processInput m = ask >>= consumeWrite m

processOutput :: Monad a => ParameterMode -> ComputerStateT a Val
processOutput m = do
  x <- consumeParam m
  tell [x]
  return x

processOutput' :: Monad a => OpCode -> ComputerStateT a Val
processOutput' (Output m) = processOutput m

jumpIf :: Monad a => (ParameterMode, ParameterMode) -> (Val -> Bool) -> ComputerStateT a ()
jumpIf (m1, m2) pred = do
  x <- consumeParam m1
  p <- consumeParam m2
  when (pred x) $ modify $ \c -> c { inst = p }

compare :: Monad a => (ParameterMode, ParameterMode, ParameterMode) -> (Val -> Val -> Bool) -> ComputerStateT a ()
compare (m1, m2, m3) pred = do
  x <- consumeParam m1
  y <- consumeParam m2
  consumeWrite m3 $ if pred x y then 1 else 0

modifyOffset :: Monad a => ParameterMode -> ComputerStateT a ()
modifyOffset m = do
  change <- consumeParam m
  off    <- gets offset
  modify (\c -> c { offset = off + change })

consumeParam :: Monad a => ParameterMode -> ComputerStateT a Val
consumeParam m = consumeInst >>= readParam m

consumeWrite :: Monad a => ParameterMode -> Val -> ComputerStateT a ()
consumeWrite m v = do
  o <- gets offset
  i <- consumeInst
  case m of
    Position  -> writeAddr i v
    Relative  -> writeAddr (i + o) v
    Immediate -> error "Immediate write not supported"

consumeInst :: Monad a => ComputerStateT a Val
consumeInst = readInst <* incrementInst

readParam :: Monad a => ParameterMode -> Addr -> ComputerStateT a Val
readParam m x = do
  offset <- gets offset
  case m of
    Position  -> readAddr x
    Immediate -> return x
    Relative  -> readAddr (x + offset)

readAddr :: Monad a => Addr -> ComputerStateT a Val
readAddr x = do
  mem <- gets mem
  case M.lookup x mem of
    Just val -> return val
    Nothing  -> return 0

readInst :: Monad a => ComputerStateT a Val
readInst = readAddr =<< inst <$> get

writeAddr :: Monad a => Addr -> Val -> ComputerStateT a ()
writeAddr addr val = do
  mem <- gets mem
  modify $ \c -> c { mem = insert addr val mem }

incrementInst :: Monad a => ComputerStateT a ()
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

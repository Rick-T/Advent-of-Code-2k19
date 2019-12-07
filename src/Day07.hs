module Day07 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad ( unless )
import Control.Monad.Trans.RWS.Lazy
import Data.List ( permutations )

data Amplifier = Amp {active :: Computer, idle :: [Computer], signal :: Int} deriving Show

type AmplifierState = RWS () () Amplifier

solution1 :: IO Int
solution1 = do
  mem <- loadMemory "input/Day07/input.txt"
  let sigForPerm ps = fst $ evalRWS (loopAmp *> gets signal) () $ initAmplifier ps mem
  return $ maximum [sigForPerm p | p <- permutations [0, 1, 2, 3, 4]]

solution2 :: IO Int
solution2 = do
  mem <- loadMemory "input/Day07/input.txt"
  let sigForPerm ps = fst $ evalRWS (loopAmp *> gets signal) () $ initAmplifier ps $ mem
  return $ maximum [sigForPerm p | p <- permutations [5, 6, 7, 8, 9]]

initAmplifier :: [Int] -> Memory -> Amplifier
initAmplifier phases m = let
  (computers, outputs) = Prelude.unzip $ Prelude.scanl (\(c, o) p -> initAmpModule p o m) (initComputer m, 0) phases
  signal = last outputs
  (active:idle) = tail computers
  in
    Amp active idle signal

initAmpModule :: Int -> Int -> Memory -> (Computer, Int)
initAmpModule phase input m = fmap head <$> execRWS (runUntil isInput *> local (const input) (runUntil isOutput)) phase $ initComputer m

loopAmp :: AmplifierState ()
loopAmp = do
  term <- stepAmp
  unless term loopAmp

stepAmp :: AmplifierState Bool
stepAmp = do
  (terminated, c', sigs) <- uncurry (runRWS $ runUntil isOutput) <$> ((,) <$> gets signal <*> gets active)
  case terminated of
    Terminated -> return True
    ConditionReached -> modify (\(Amp _ cs sig) -> Amp (head cs) (tail cs ++ [c']) $ last sigs) *> return False
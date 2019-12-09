module Day07 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad ( unless )
import Control.Monad.Trans.RWS.Lazy
import Data.Functor ( ($>) )
import Data.List ( permutations )

data Amplifier = Amp {active :: Computer, idle :: [Computer], signal :: Integer} deriving Show

type AmplifierState = RWS () [Integer] Amplifier

part1 :: IO Integer
part1 = do
  mem <- loadMemory "input/Day07.txt"
  let sigForPerm ps = fst $ evalRWS (loopAmp *> gets signal) () $ initAmplifier ps mem
  return $ maximum [sigForPerm p | p <- permutations [0..4]]

part2 :: IO Integer
part2 = do
  mem <- loadMemory "input/Day07.txt"
  let sigForPerm ps = fst $ evalRWS (loopAmp *> gets signal) () $ initAmplifier ps mem
  return $ maximum [sigForPerm p | p <- permutations [5..9]]

initAmplifier :: [Integer] -> Memory -> Amplifier
initAmplifier phases m = let
  (_:active:idle, signal) = fmap last . unzip $ scanl (\(c, o) p -> initAmpModule p o m) (initComputer m, 0) phases
  in
    Amp active idle signal

initAmpModule :: Integer -> Integer -> Memory -> (Computer, Integer)
initAmpModule phase input m = fmap head <$> execRWS (runUntil isInput *> local (const input) (runUntil isOutput)) phase $ initComputer m

loopAmp :: AmplifierState ()
loopAmp = do
  term <- stepAmp
  unless term loopAmp

stepAmp :: AmplifierState Bool
stepAmp = do
  (terminated, c', sigs) <- uncurry (runRWS $ runUntil isOutput) <$> ((,) <$> gets signal <*> gets active)
  cs <- gets idle
  let sig' = last sigs
  let cs' = cs ++ [c']
  case terminated of
    Terminated -> return True
    ConditionReached -> tell [sig'] *> put (Amp (head cs') (tail cs') sig') $> False
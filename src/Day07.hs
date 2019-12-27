module Day07 (part1, part2) where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad (unless)
import Control.Monad.RWS.Strict
import Data.Functor (($>))
import Data.List (permutations)

data Amplifier = Amp {active :: Computer, idle :: [Computer], signal :: Val} deriving Show

type AmplifierState = RWS () [Val] Amplifier

part1 :: IO Val
part1 = solve [0 .. 4]

part2 :: IO Val
part2 = solve [5 .. 9]

solve :: [Val] -> IO Val
solve phases = do
  c <- loadComputer "input/Day07.txt"
  let sigForPerm ps = fst $ evalRWS (loopAmp *> gets signal) () $ initAmplifier ps c
  return $ maximum [ sigForPerm p | p <- permutations phases ]

initAmplifier :: [Val] -> Computer -> Amplifier
initAmplifier phases c =
  let (_ : active : idle, signal) = fmap last . unzip $ scanl (\(_, o) p -> initAmpModule p o c) (c, 0) phases in Amp active idle signal

initAmpModule :: Val -> Val -> Computer -> (Computer, Val)
initAmpModule phase input c = fmap head <$> execRWS (runUntil isInput *> local (const input) (runUntil isOutput)) phase $ c

loopAmp :: AmplifierState ()
loopAmp = do
  term <- stepAmp
  unless term loopAmp

stepAmp :: AmplifierState Bool
stepAmp = do
  (terminated, c', sigs) <- uncurry (runRWS $ runUntil isOutput) <$> ((,) <$> gets signal <*> gets active)
  cs                     <- gets idle
  let sig' = last sigs
  let cs'  = cs ++ [c']
  case terminated of
    Terminated       -> return True
    ConditionReached -> tell [sig'] *> put (Amp (head cs') (tail cs') sig') $> False

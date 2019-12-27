module Day23 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Control.Monad.State.Lazy
import Control.Monad.RWS.Strict
import Data.IntMap.Strict as M
import Data.Sequence as S

data Network = Net { network :: IntMap Computer, packetQueue :: Seq Packet, nat :: Maybe Packet} deriving Show

type NetworkState = State Network

type Packet = (Int, Int, Int)

part1 :: IO Packet
part1 = head . natOutput <$> loadComputer "input/Day23.txt"

part2 :: IO (Maybe Packet)
part2 = firstDup . natOutput <$> loadComputer "input/Day23.txt"

firstDup :: Eq a => [a] -> Maybe a
firstDup []  = Nothing
firstDup [a] = Nothing
firstDup (a : b : cs)
  | a == b    = Just a
  | otherwise = firstDup $ b : cs

natOutput :: Computer -> [Packet]
natOutput = evalState stepNetwork . initNetwork

stepNetwork :: NetworkState [Packet]
stepNetwork = do
  mPckt <- consumePacket
  case mPckt of
    Nothing -> do
      pckt <- searchPacket
      case pckt of
        [] -> sendNat
        ps -> do
          addPackets ps
          stepNetwork
    Just pckt -> do
      handlePacket pckt
      stepNetwork

sendNat :: NetworkState [Packet]
sendNat = do
  toSend <- gets nat
  case toSend of
    Nothing        -> stepNetwork
    Just (_, x, y) -> do
      addPacket (0, x, y)
      ((0, x, y) :) <$> stepNetwork

searchPacket :: NetworkState [Packet]
searchPacket = searchPacket' 0

searchPacket' :: Int -> NetworkState [Packet]
searchPacket' ptr = do
  pckts <- receivePackets ptr $ runUntil isInput
  case pckts of
    [] -> if ptr == 49 then return [] else searchPacket' (ptr + 1)
    _  -> return pckts

handlePacket :: Packet -> NetworkState ()
handlePacket (255, x, y) = modify $ \n -> n { nat = Just (0, x, y) }
handlePacket (d  , x, y) = addPackets =<< receivePackets d (feedInput [x, y])

receivePackets :: Int -> ComputerState a -> NetworkState [Packet]
receivePackets d comm = do
  output <- snd <$> runComputer d comm
  case output of
    []           -> return []
    partialPckts -> do
      let missingData = (3 - (Prelude.length partialPckts `mod` 3)) `mod` 3
      (_, trailing) <- runComputer d (nextOutputs missingData)
      return $ toPackets $ partialPckts ++ trailing

consumePacket :: NetworkState (Maybe Packet)
consumePacket = do
  q <- gets packetQueue
  case q of
    Empty         -> return Nothing
    (pckt :<| q') -> do
      modify $ \n -> n { packetQueue = q' }
      return $ Just pckt

addPackets :: [Packet] -> NetworkState ()
addPackets = mapM_ addPacket

addPacket :: Packet -> NetworkState ()
addPacket packet = do
  q <- gets packetQueue
  modify $ \n -> n { packetQueue = q |> packet }

runComputer :: Int -> ComputerState a -> NetworkState (a, [Val])
runComputer d com = do
  net <- gets network
  let c             = net ! d
  let (a, c', outs) = runRWS com (-1) c
  modify $ \n -> n { network = M.insert d c' net }
  return (a, outs)

initNetwork :: Computer -> Network
initNetwork c =
  let network = M.fromList $ fmap (\(d, c) -> (d, fst $ execRWS (feedInput [d]) (-1) c)) $ Prelude.zip [0 .. 49] $ repeat c in Net network mempty Nothing

toPackets :: [Val] -> [Packet]
toPackets []                 = []
toPackets (d : x : y : rest) = (d, x, y) : toPackets rest

destination :: Packet -> Int
destination (d, _, _) = d

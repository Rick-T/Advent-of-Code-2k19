module Common.Dijkstra where

import Control.Monad.State
import Data.Hashable
import Data.HashMap.Lazy as M
import Data.HashPSQ as Q

data DSearch a b = DSearch { queue :: HashPSQ a Int b, visited :: HashMap a (Int, b) }

type DijkstraState a b = State (DSearch a b)

type Dijkstra a b c = (a -> [(a, Int)]) -> (a -> b -> b) -> DijkstraState a b c

runDijkstra :: (Eq a, Ord a, Hashable a) => Dijkstra a b c -> (a -> [(a, Int)]) -> (a -> b -> b) -> a -> b -> c
runDijkstra goal edges accumulator start startAcc = evalState (goal edges accumulator) (DSearch (Q.singleton start 0 startAcc) mempty)

searchMany :: (Eq a, Ord a, Hashable a) => [a] -> Dijkstra a b [Maybe (Int, b)]
searchMany ends edges accumulator = forM ends $ \end -> searchOne end edges accumulator

searchOne :: (Eq a, Ord a, Hashable a) => a -> Dijkstra a b (Maybe (Int, b))
searchOne end edges accumulator = do
    searchUntil (== end) edges accumulator
    DSearch _ v <- get
    return $ M.lookup end v

searchAll :: (Eq a, Ord a, Hashable a) => Dijkstra a b ()
searchAll e a = searchUntil (const False) e a *> return ()

searchUntil :: (Eq a, Ord a, Hashable a) => (a -> Bool) -> Dijkstra a b (Maybe (a, Int, b))
searchUntil endCond edges accumulator = do
    (DSearch q v) <- get
    case minView q of
        Nothing -> return Nothing
        Just (a, value, acc, q')
            | a `M.member` v -> modify (\ds -> ds { queue = q' }) *> searchUntil endCond edges accumulator
            | otherwise -> do
                let acc'      = accumulator a acc
                let v'        = M.insert a (value, acc') v
                let neighbors = edges a
                let
                    q'' =
                        Prelude.foldr
                                (\(node, cost) queue ->
                                    if node `M.member` v' then queue else insertIfLess node (value + cost) acc' queue
                                )
                                q'
                            $ edges a
                put $ DSearch q'' v'
                if endCond a
                    then return $ Just (a, value, acc)
                    else searchUntil endCond edges accumulator

insertIfLess :: (Eq a, Ord a, Hashable a) => a -> Int -> b -> HashPSQ a Int b -> HashPSQ a Int b
insertIfLess node prio acc queue = case Q.lookup node queue of
    Nothing         -> Q.insert node prio acc queue
    Just (prio', _) -> if prio' < prio then queue else Q.insert node prio acc queue

module Day19 where

import Paths_Advent_of_Code_2k19
import Common.Intcode
import Common.GridMovement
import Common.Util
import Control.Monad.RWS.Lazy
import Control.Monad.State
import Data.List
import Data.Map.Strict as M
import Data.Ord

type Slope = (Int, Int)

part1 :: IO Val
part1 = do
    computer <- loadComputer "input/Day19.txt"
    return $ length $ toList $ M.filter (== 1) $ initialGrid computer

part2 :: IO Val
part2 = do
    c <- loadComputer "input/Day19.txt"
    let
        (x, y) = head
            [ (x, y)
            | slopes@(m1, _) <- iterate (improveSlopes c) $ guessSlopes $ initialGrid c
            , let xCand  = xFor slopes
            , let xCand' = xFor $ improveSlopes c slopes
            , abs (xCand - xCand') < 5
            , x <- [xCand' - 10 .. xCand' + 10]
            , let y = upperY m1 x c
            , (x, y) `fits` c
            ]
    return $ 10000 * x + y

fits :: Position -> Computer -> Bool
fits (x, y) c = sendRobot (x, y + 99) c == 1 && sendRobot (x, y + 100) c == 0 && sendRobot (x + 99, y) c == 1 && sendRobot (x + 100, y) c == 0

upperY :: Slope -> Val -> Computer -> Val
upperY (dy, dx) x c = let y1 = 1 + ((99 + fromIntegral x) * dy) `div` dx in head (dropWhile (\y -> sendRobot (x + 99, y) c == 1) [y1, y1 - 1 .. 0]) + 1

sendRobot :: Position -> Computer -> Val
sendRobot (x, y) computer = head $ snd $ evalRWS (feedInput [x, y] *> runProgram) 0 computer

improveSlopes :: Computer -> (Slope, Slope) -> (Slope, Slope)
improveSlopes c slopes@(m1, m2) =
    let
        x   = xFor slopes
        y   = yFor (middle m1 m2) x
        m1' = toSlope $ findBoundary East c (x, y)
        m2' = toSlope $ findBoundary South c (x, y)
    in (m1', m2')

middle :: Slope -> Slope -> Slope
middle (num1, denom1) (num2, denom2) = (num1 * denom2 + num2 * denom1, 2 * denom1 * denom2)

findBoundary :: Direction -> Computer -> Position -> Position
findBoundary dir c pos
    | sendRobot (move pos dir) c == 0 = pos
    | otherwise                       = findBoundary dir c (move pos dir)

initialGrid :: Computer -> Map Position Int
initialGrid computer = fromList [ ((x, y), sendRobot (x, y) computer) | x <- [0 .. 49], y <- [0 .. 49] ]

guessSlopes :: Map Position Int -> (Slope, Slope)
guessSlopes map = (toSlope $ furthestUpper map, toSlope $ furthestLower map)

toSlope :: Position -> Slope
toSlope (x, y) = (y, x)

xFor :: (Slope, Slope) -> Int
xFor ((num1, denom1), (num2, denom2)) = (99 * denom2 * (denom1 + num1)) `div` (denom1 * num2 - denom2 * num1)

yFor :: Slope -> Int -> Int
yFor (num, denom) x = (num * x) `div` denom

furthestUpper :: Map Position Int -> Position
furthestUpper m = minimumBy (comparing (\(x, y) -> (-x, y))) $ keys $ M.filter (== 1) m

furthestLower :: Map Position Int -> Position
furthestLower m = minimumBy (comparing (\(x, y) -> (-y, x))) $ keys $ M.filter (== 1) m

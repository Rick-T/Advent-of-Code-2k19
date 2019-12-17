module Common.GridMovement where

type Position = (Int, Int)

data Direction = North | East | South | West deriving Show

move :: Position -> Direction -> Position
move = moveN 1

moveN :: Int -> Position -> Direction -> Position
moveN n (x, y) North = (x, y - n)
moveN n (x, y) East  = (x + n, y)
moveN n (x, y) South = (x, y + n)
moveN n (x, y) West  = (x - n, y)

turnRight :: Direction -> Direction
turnRight North = East
turnRight East  = South
turnRight South = West
turnRight West  = North

turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West  = South
turnLeft South = East
turnLeft East  = North

turnAround :: Direction -> Direction
turnAround North = South
turnAround South = North
turnAround East  = West
turnAround West  = East

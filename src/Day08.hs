module Day08 ( part1, part2 ) where

import Paths_Advent_of_Code_2k19
import Common.Util ( mapBoth )
import Data.Foldable ( maximumBy )
import Data.Function ( on )
import Data.List
import Data.List.Split

type Image = [Layer]

type Layer = [Pixel]

data Pixel = Black | White | Transparent deriving Eq

instance Monoid Pixel where
    mempty = Transparent

instance Semigroup Pixel where
    Transparent <> p = p
    p <> _ = p

instance Show Pixel where
    show White = "\x2591"
    show Black = "\x2588"
    show Transparent = " "
    showList = foldl (\f p -> f . shows p) id

part1 :: IO Int
part1 = do
    image <- loadImage
    return $ uncurry (*) $ mapBoth length $ partition (== White) $ maximumBy (compare `on` length) $ filter (/= Black) <$> image

part2 :: IO ()
part2 = do
    image <- loadImage
    mapM_ print $ chunksOf width $ mconcat <$> transpose image

loadImage :: IO Image
loadImage = do
    image <- readFile =<< getDataFileName "input/Day08.txt"
    return $ chunksOf resolution $ toPixel <$> image 

toPixel :: Char -> Pixel
toPixel '0' = Black
toPixel '1' = White
toPixel '2' = Transparent

resolution :: Int
resolution = width*height

width :: Int
width = 25

height :: Int
height = 6

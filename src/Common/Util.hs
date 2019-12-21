module Common.Util where

import Data.List.Split (chunksOf, splitOn)
import Data.Map.Strict as M (Map(..), findWithDefault, fromList, keys, map)

everySecond :: [a] -> [a]
everySecond []             = []
everySecond [a           ] = [a]
everySecond (a : b : rest) = a : everySecond rest

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

toDigits :: Show a => Read a => Integral a => a -> [a]
toDigits = fmap (read . return) <$> show

toNumber :: Integral a => [a] -> a
toNumber = foldl (\total p -> 10 * total + p) 0

printMap :: Integral i => (a -> Char) -> Map (i, i) a -> IO ()
printMap = printMapWithDefault' ' '

printMapWithDefault :: Integral i => a -> (a -> Char) -> Map (i, i) a -> IO ()
printMapWithDefault def toReadable = printMapWithDefault' (toReadable def) toReadable

printMapWithDefault' :: Integral i => Char -> (a -> Char) -> Map (i, i) a -> IO ()
printMapWithDefault' def toReadable grid = do
    let ks           = keys grid
    let (xmin, ymin) = (minimum $ fst <$> ks, minimum $ snd <$> ks)
    let (xmax, ymax) = (maximum $ fst <$> ks, maximum $ snd <$> ks)
    mapM_ putStrLn $ chunksOf
        (fromIntegral xmax - fromIntegral xmin + 1)
        [ tile | y <- [ymin .. ymax], x <- [xmin .. xmax], let tile = findWithDefault def (x, y) $ M.map toReadable grid ]

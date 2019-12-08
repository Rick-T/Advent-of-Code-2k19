module Common.Util where

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

toDigits :: Int -> [Int]
toDigits = fmap (read . return) <$> show

toNumber :: [Int] -> Int
toNumber = foldl (\total p -> 10 * total + p) 0
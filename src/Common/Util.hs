module Common.Util where

mapBoth :: (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) = (f x, f y)

toDigits :: Show a => Read a => Integral a => a -> [a]
toDigits = fmap (read . return) <$> show

toNumber :: Integral a => [a] -> a
toNumber = foldl (\total p -> 10 * total + p) 0

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Day22 where

import Paths_Advent_of_Code_2k19
import Data.Semigroup
import Text.Read
import GHC.TypeNats hiding (Mod)

newtype Mod (n :: Nat ) = Mod Integer

data Shuffle a = Shuffle a a deriving (Show)

instance KnownNat n => Num (Mod n) where
    fromInteger i = let x = Mod $ i `mod` fromIntegral (natVal x) in x
    Mod a + Mod b = fromInteger (a + b)
    Mod a * Mod b = fromInteger (a * b)
    negate (Mod a) = fromInteger (negate a)
    abs (Mod a) = fromInteger a
    signum (Mod 0) = 0
    signum (Mod _) = 1

instance KnownNat n => Fractional (Mod n) where
    recip x = x ^ (natVal x - 2)
    fromRational = undefined

instance KnownNat n => Show (Mod n) where
    show x@(Mod i) = show i

instance (KnownNat n) => Read (Mod n) where
    readPrec = Mod <$> (readPrec :: ReadPrec Integer)

instance Num a => Monoid (Shuffle a) where
    mempty = Shuffle 1 0

instance Num a => Semigroup (Shuffle a) where
    (Shuffle m b) <> (Shuffle m' b') = Shuffle (m * m') (m' * b + b')

part1 :: IO (Mod 10007)
part1 = do
    s <- loadShuffle
    return $ apply s 2019

part2 :: IO (Mod 119315717514047)
part2 = do
    s <- stimes 101741582076661 <$> loadShuffle
    return $ solve s 2020

apply :: Num a => Shuffle a -> a -> a
apply (Shuffle m b) index = m * index + b

solve :: Fractional a => Shuffle a -> a -> a
solve (Shuffle m b) index = (index - b) / m

loadShuffle :: (Read a, Num a) => IO (Shuffle a)
loadShuffle = mconcat . fmap parseShuffle . lines <$> (readFile =<< getDataFileName "input/Day22.txt")

parseShuffle :: (Read a, Num a) => String -> Shuffle a
parseShuffle "deal into new stack" = Shuffle (-1) (-1)
parseShuffle s@('d' : _)           = Shuffle (read $ drop 20 s) 0
parseShuffle s@('c' : _)           = Shuffle 1 $ negate $ read $ drop 4 s

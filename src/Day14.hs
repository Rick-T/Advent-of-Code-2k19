module Day14 where

import Paths_Advent_of_Code_2k19
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.List.Split
import Data.Map.Strict as M

type Chemical = String

type ChemicalAmount = (Chemical, Int)

type Reaction = ([ChemicalAmount], ChemicalAmount)

type AlchemyBook = Map Chemical Reaction

type Inventory = Map Chemical Int

type ReactionState = StateT Inventory (Reader AlchemyBook)

part1 :: IO Int
part1 = oresFor ("FUEL", 1) <$> loadAlchemyBook

part2 :: IO (Maybe Int)
part2 = fuelAmount 1000000000000 <$> loadAlchemyBook

fuelAmount :: Int -> AlchemyBook -> Maybe Int
fuelAmount numOres book = searchBiggestWhich 0 numOres (\i -> oresFor ("FUEL", i) book - numOres < 0)

oresFor :: ChemicalAmount -> AlchemyBook -> Int
oresFor c = runReader (evalStateT (produce c *> getOre) mempty)

getOre :: ReactionState Int
getOre = gets $ findWithDefault 0 "ORE"

searchBiggestWhich :: Int -> Int -> (Int -> Bool) -> Maybe Int
searchBiggestWhich lower upper pred | lower == upper = if pred lower then Just lower else Nothing
searchBiggestWhich lower upper pred =
    let midpoint = (lower + upper + 1) `div` 2
    in if pred midpoint then searchBiggestWhich midpoint upper pred else searchBiggestWhich lower (midpoint - 1) pred

produce :: ChemicalAmount -> ReactionState ()
produce ("ORE", n  ) = modify $ insertWith (+) "ORE" n
produce (chem , num) = do
    available <- gets $ findWithDefault 0 chem
    if available >= num
        then modify $ insert chem (available - num)
        else do
            modify $ insertWith (-) chem available
            (ingredients, (c, n)) <- lift $ ingredientsFor (chem, num - available)
            modify $ insertWith (+) c n
            forM_ ingredients produce

ingredientsFor :: ChemicalAmount -> Reader AlchemyBook ([ChemicalAmount], ChemicalAmount)
ingredientsFor (chem, n) = do
    book <- ask
    let reaction = M.lookup chem book
    case reaction of
        Nothing -> error $ "Don't know how to produce " ++ chem
        Just (ingredients, (_, numResult)) ->
            let
                reactionsAmount = (1 + ((n - 1) `div` numResult))
                leftOver        = (reactionsAmount * numResult) - n
            in return (fmap (* reactionsAmount) <$> ingredients, (chem, leftOver))

toChemicalAmount :: String -> ChemicalAmount
toChemicalAmount s = let [num, chem] = splitOn " " s in (chem, read num)

toReaction :: String -> Reaction
toReaction s =
    let
        [inputsL, output] = splitOn " => " s
        inputs            = toChemicalAmount <$> splitOn ", " inputsL
    in (inputs, toChemicalAmount output)

alchemyBook :: [Reaction] -> AlchemyBook
alchemyBook rs = fromList $ [ (chem, reac) | reac <- rs, let chem = fst $ snd reac ]

loadAlchemyBook :: IO AlchemyBook
loadAlchemyBook = do
    f <- readFile =<< getDataFileName "input/Day14.txt"
    return $ alchemyBook $ fmap toReaction $ lines f

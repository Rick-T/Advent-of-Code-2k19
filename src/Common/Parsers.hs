module Common.Parsers where

import Text.Parsec (optionMaybe)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (oneOf, char, digit, letter)
import Text.Parsec.Combinator(many1, sepBy1)

num :: Read a => Integral a => Parser a
num = do
    sign <- optionMaybe $ char '-'
    n <- many1 digit
    return . read $ case sign of
        Nothing -> n
        Just _ -> '-':n

import Control.Applicative
import Data.Char

import AParser

-- 1: parser x -> parser [x]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore pa = Parser { runParser = run }
    where
      loop (Just (a, input)) prev_input acc = loop (runParser pa input) input (acc ++ [a])
      loop Nothing prev_input acc = Just (acc, prev_input)
      run input = loop (runParser pa input) input []

oneOrMore  :: Parser a -> Parser [a]
oneOrMore pa = Parser { runParser = run }
    where
      run input =  do (a1, rest1) <- runParser pa input;
                      (a2, rest2) <- runParser (zeroOrMore pa) rest1;
                      return (a1:a2, rest2)

-- 2: parsing tokens

spaces :: Parser String
spaces = Parser { runParser = run }
    where
      -- run input = (runParser (oneOrMore (satisfy isSpace)) input) >>= (\(spaces, rest) -> Just (" ", rest))
      run input = do (spaces, rest) <- runParser (oneOrMore $ satisfy isSpace) input
                     return (" ", rest) -- squash all parsed space into one

ident :: Parser String
ident = Parser { runParser = run }
    where
      run input = do (initial, rest1) <- runParser (satisfy isAlpha) input
                     (body, rest2) <- runParser (oneOrMore $ satisfy isAlphaNum) rest1
                     return (initial:body, rest2)

-- 3: parsing sexps

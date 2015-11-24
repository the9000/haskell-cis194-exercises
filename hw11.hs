import Control.Applicative
import Control.Monad (liftM)
import Data.Char

import AParser

import Hw10

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
                     (body, rest2) <- runParser (zeroOrMore $ satisfy isAlphaNum) rest1
                     return (initial:body, rest2)

-- 3: parsing sexps

type Ident = String

data Atom = N Integer | I Ident
            deriving Show

data SExpr = A Atom | Comb [SExpr]
             deriving Show

(@>) :: Parser a -> (a -> b) -> Parser b
(@>) pa wrapper = Parser { runParser = run } where
    run input = (runParser pa input) >>= \t -> Just  $ first wrapper t

atomP :: Parser Atom
atomP = (posInt @> N) <|> (ident @> I)

parseSExpr :: Parser SExpr
parseSExpr = parser where
    optSpace = zeroOrMore (satisfy isSpace)
    lparenP = satisfy (== '(')
    rparenP = satisfy (== ')')
    parenthesizedP = optSpace *> lparenP *> 
                     (zeroOrMore (optSpace *> parseSExpr)) <* optSpace <* rparenP 
    parser = atomP @> A <|> (parenthesizedP @> Comb)

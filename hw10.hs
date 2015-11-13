import Control.Applicative
import Data.Char

import AParser

-- 1: make Parser a Functor

first :: (a -> b) -> (a, x) -> (b, x)
first f (a, x) = ((f a), x)

instance Functor Parser where
    fmap  f pa = Parser { runParser = transformed_parsing }
        where
          transformed_parsing input = (runParser pa input) >>= apply_transform
          apply_transform (a, rest) = Just(f a, rest)
          -- or: apply_transform = Just . first f


-- 2: make Parser an Applicastive

instance Applicative Parser where
    pure a = Parser { runParser = \input -> Just(a, input) }
    (<*>) parser_f parser_a = Parser { runParser = transformed_parsing }
        where
          transformed_parsing input = (runParser parser_f input) >>= apply_transform
          apply_transform (f, more_input) = (runParser parser_a more_input) >>= (return . first f)

-- 3: making higher-level parsers

abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\n1 _ n2 -> [n1, n2]) <$> posInt <*> (char ' ') <*> posInt

-- 4: 

instance Control.Applicative.Alternative Parser where
    empty = Parser { runParser = const Nothing }
    (<|>) pa pb = Parser { runParser = \input -> (runParser pa input) <|> (runParser pb input) }

-- 5:

upperCharStr = show <$> (satisfy isUpper)
intStr = show <$> posInt

intOrUppercase :: Parser ()
intOrUppercase = const () <$> (upperCharStr <|> intStr)

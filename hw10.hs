import Control.Applicative

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

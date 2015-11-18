import Control.Applicative

import AParser

-- 1: parser x -> parser [x]

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore pa = Parser { runParser = run }
    where
      loop (Just (a, input)) prev_input acc = loop (runParser pa input) input (acc ++ [a])
      loop Nothing prev_input acc = Just (acc, prev_input)
      run input = loop (runParser pa input) input []

oneOrMore  :: Parser a -> Parser [a]
oneOrMore = undefined

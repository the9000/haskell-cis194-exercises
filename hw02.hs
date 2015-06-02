-- CIS 194 Homework 2

module Log where

import Control.Applicative
import Text.Read
-- import Control.Monad

data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)


parseMessage :: String -> LogMessage
parseMessage s = maybe (Unknown s) id msg
  where msg = parseInfo js <|> parseWarning js <|> parseError js
        js = Just ((), s)

parseInfo js = rs >>= (\(((_, _), timestamp), text) ->
                        return $ LogMessage Info timestamp text)
  where rs = js ||> takeIdent "I" ||> takeInt 

parseWarning js = rs >>= (\(((_, _), timestamp), text) ->
                           return $ LogMessage Warning timestamp text)
  where rs = js ||> takeIdent "W" ||> takeInt 

parseError js = rs >>= (\((((_, _), severity), timestamp), text) -> 
                          return $ LogMessage (Error severity) timestamp text)
  where rs = js ||> takeIdent "E" ||> takeInt ||> takeInt 


parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree 
insert lm Leaf = Node Leaf lm Leaf 
insert lm (Node left current right) = Node new_left current new_right
  where (new_left, new_right) = if timestamp < current_timestamp then (insert lm left, right)
                                else (left, insert lm right)
        (LogMessage _ current_timestamp _) = current
        (LogMessage _ timestamp _) = lm
                                  

build :: [LogMessage] -> MessageTree
build lms = build' lms Leaf
build' [] tree = tree
build' (m:ms) tree = build' ms (insert m tree)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = inOrder left ++ [lm] ++ inOrder right


isSerious (LogMessage (Error sev) _ _) = sev >= 50
--isSerious (LogMessage Warning _ _) = True
isSerious _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ text) -> text) . filter isSerious . inOrder . build


-- | @testParse p n f@ tests the log file parser @p@ by running it
--   on the first @n@ lines of file @f@.
testParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
testParse parse n file = take n . parse <$> readFile file

-- | @testWhatWentWrong p w f@ tests the log file parser @p@ and
--   warning message extractor @w@ by running them on the log file
--   @f@.
testWhatWentWrong :: (String -> [LogMessage])
                  -> ([LogMessage] -> [String])
                  -> FilePath
                  -> IO [String]
testWhatWentWrong parse whatWentWrong file
  = whatWentWrong . parse <$> readFile file


---
isSpace = (' ' ==)
isNotSpace = (' ' /=) 

justIf pred s = if pred s then Just s else Nothing

takeInt s = readMaybe s :: Maybe Int

takeIdent name s = justIf (== name) s

takeWord = justIf ((> 0) . length)

(||>) :: Maybe (a1, String) -> (String -> Maybe a2) -> Maybe ((a1, a2), String)
(||>) Nothing _ = Nothing
(||>) (Just (a1, s)) f = (takeWord word >>= f) >>= (\a2 -> Just ((a1, a2), rest))
  where (word, rest) = span isNotSpace $ dropWhile isSpace s


{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, TypeSynonymInstances #-}

import Data.Monoid
import Data.List (intersperse)
import Control.Monad (join)

import Sized
import Buffer

-- Join List from assignment
data JoinList m a = Empty
   | Single m a
   | Append m (JoinList m a) (JoinList m a)
   deriving (Eq, Show)

-- #1: appending join lists
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty j = j
(+++) j Empty = j
(+++) j1 j2 = Append (mappend (tag j1) (tag j2)) j1 j2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- for easy interactive tests:
mkSingle a = Single (Size 1) a

-- #2: indexing

-- 2.1

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ i (Append s _ _) | i >= (getSize . size $ s) = Nothing  -- over right bound
indexJ i (Single s x) | i == 0 = Just x

-- Here we expect that Empty can't happen in an Append at all.
indexJ i (Append _ left right) = if i < left_size then indexJ i left
                                 else indexJ (i - left_size) right
    where
      left_size = jlSize left
indexJ i _ = Nothing

-- from task text, just to simplify testing
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- 2.2

dropJ :: (Monoid b, Sized b) => Int -> JoinList b a -> JoinList b a
-- jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ 0 j = j -- drop nothing
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty

dropJ n x@(Append s j_left j_right)
    | n < 0 = error ("Error: n == " ++ (show n))
    | n >= jlSize x = Empty
    | n < left_size = (dropJ n j_left) +++ j_right
    | n == left_size = j_right
    | otherwise = dropJ (n - left_size) j_right
    where
      left_size = jlSize j_left
      right_size = jlSize j_right


-- 2.3

takeJ :: (Monoid b, Sized b) => Int -> JoinList b a -> JoinList b a
tskeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ 1 x@(Single m a) = x
takeJ n x@(Append s j_left j_right)
    | n >= full_size = x
    | n < left_size = takeJ n j_left
    | n == left_size = j_left
    | n >= left_size = j_left +++ (takeJ (n - left_size) j_right)
    where
      left_size = jlSize j_left
      right_size = jlSize j_right
      full_size = jlSize x


jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize Empty = 0
jlSize x = getSize . size . tag $ x


-- 3: scrabble score

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

intScore :: Score -> Int
intScore (Score n) = n

-- TODO: use a faster lookup, e.g. via an array or a treemap
score :: Char -> Score
score 'a' = Score 1
score 'x' = Score 10
score _ = 0

instance Monoid Score where
    mempty = Score 0
    mappend = (+)

scoreString :: String -> Score
scoreString = foldr mappend mempty . map score  


scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s


-- Poor man's unit testing 'framework' (to avoid setting up HUnit)

assert :: Bool -> Maybe String
assert True = Nothing
assert _ = Just "Failed"

assertEquals :: (Eq a, Show a) => a -> a -> Maybe String
assertEquals x y 
    | x == y = Nothing
    | otherwise = Just ("Expected " ++ (show x) ++ ", got " ++ (show y))


-- Sort of tests

a1 = mkSingle 'a'
a2 = a1 +++ mkSingle 'b'
a3_l = mkSingle '@' +++ a2
a3_r = a2 +++ mkSingle 'c'

testSuite = undefined

-- we ignore the fact that a balanced tree would work better, and load a linear list of lines.

instance Buffer (JoinList (Score, Size) String) where
  toString     = join . intersperse "\n". jlToList
  fromString   = foldr (+++) Empty . map makeSingle . lines 
    where makeSingle s = Single (scoreString s, Size 1) s
  line         = indexJ
  replaceLine n new_line buf = undefined -- replaceJ n new_line b
  numLines     = jlSize
  value        = intScore . fst . tag


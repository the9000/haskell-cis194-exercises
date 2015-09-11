{- -}

import Data.Monoid
import Sized

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

-- #2: indexing

-- 2.1

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ i (Append s _ _) | i >= (getSize . size $ s) = Nothing  -- over right bound
indexJ i (Single s x) | i == 0 = Just x

-- Here we expect thet Empty can't happen in an Append at all.
indexJ i (Append _ left right) = if i < left_size then indexJ i left
                                 else indexJ (i - left_size) right
    where
      left_size = getSize . size . tag $ left
indexJ i _ = Nothing

-- from task text, just to simplify testing
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- 2.2

{- 
NOTE: had to drop the limitation of (Monoif b, Sized b) => JoinList a b
because doing math with sizes requres them tobe Size, not just Sized.
Being a Monoid does not help: it gives addition, and we need subtraction.
Also we receive an integer parameter. We can't even emulate addition (and
via it subtracion) of b and Int via Peano arithmetic, for Monoid lacks a unity.
-}

dropJ :: Int -> JoinList Size a -> JoinList Size a
-- jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ 0 j = j -- drop nothing
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty

dropJ n x@(Append s j_left j_right)
    | n >= full_size = Empty
    | left_size < n = Append (Size (full_size - n)) (dropJ (n - left_size) j_left) j_right
    | left_size == n = j_right
    | otherwise = dropJ (n - full_size) j_right
    where
      left_size = jlSize j_left
      right_size = jlSize j_right
      full_size = jlSize x


-- 2.3

takeJ :: Int -> JoinList Size a -> JoinList Size a
tskeJ _ Empty = Empty
takeJ 0 _ = Empty
takeJ 1 x@(Single m a) = x
takeJ n x@(Append s j_left j_right)
    | n >= full_size = x
    | n < left_size = takeJ n j_left
    | n == left_size = j_left
    | n >= left_size = (Append new_small_size j_left (takeJ (n - left_size) j_right))
    where
      new_small_size = Size (full_size - n)
      left_size = jlSize j_left
      right_size = jlSize j_right
      full_size = jlSize x


jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize Empty = 0
jlSize x = getSize . size . tag $ x

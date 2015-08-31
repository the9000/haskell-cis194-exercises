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


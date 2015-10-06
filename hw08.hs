-- Homework 8: IO

import Data.Monoid
import Data.Tree
import Data.List (foldl')
import Control.Monad (join)

import Employee

-- 1

-- does no checks as asked by the task.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL guests fun) = GL (e:guests) (fun + empFun e)


instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL g1 f1) (GL g2 f2) = GL (g1 ++ g2) (f1 + f2)


moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2) = if f1 >= f2 then g1 else g2


-- 2

treeFold :: (b -> a -> b) -> b -> Tree a -> b 
treeFold func init tree = foldl' func init $ flattened tree 
    where
      flattened (Node {rootLabel=r, subForest=sf}) = r:(join $ map flattened sf)

-- REPL helpers
mkNode r sf = Node {rootLabel=r, subForest=sf}

-- Homework 8: IO

import Data.Monoid
import Data.Tree
import Data.List (foldl', maximumBy)
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

-- boss -> [(best possible list with subtree boss), (bpl without)] -> (bpl with, bpl without)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bpls = (our_bpl_with, our_bpl_without) where
    (bpls_with, bpls_without) = unzip bpls
    our_bpl_with = glCons boss (mconcat bpls_without) 
    our_bpl_without = mconcat bpls_with

-- 4
-- A bit more compact than 
-- tree level -> (bpl with node's boss, bpl without node's boss)
onLevel :: Tree Employee -> (GuestList, GuestList)
onLevel (Node boss []) = (GL [boss] (empFun boss), GL [] 0)
onLevel (Node boss subs) = (bpl_with_boss, bpl_without_boss) where
    (bpls_with, bpls_without) = unzip $ map onLevel subs                 
    bpl_with_boss = glCons boss (mconcat bpls_without)
    bpl_without_boss = mconcat bpls_with                              


maxFun :: Tree Employee -> GuestList
maxFun = uncurry max $ onLevel


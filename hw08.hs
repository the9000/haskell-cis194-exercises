-- Homework 8: IO

import Data.Monoid

import Employee

-- 1

-- does no checks as asked by the task.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL guests fun) = GL (e:guests) (fun + empFun e)


-- monoid GL
instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL g1 f1) (GL g2 f2) = GL (g1 ++ g2) (f1 + f2)

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{- hw 05: DSLs -}

import qualified ExprT
import Parser
import qualified StackVM

import Control.Applicative

import qualified Data.Map as M

{- 1 -}

eval :: ExprT.ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add a b) = (+) (eval a) (eval b)
eval (ExprT.Mul a b) = (*) (eval a) (eval b)

{- 2 -}

evalStr :: String -> Maybe Integer
-- three working implementations
-- evalStr s = parseExp Lit Add Mul s >>= (\x -> Just (eval x))
-- evalStr s = Just eval <*> parseExp Lit Add Mul s
evalStr s = eval <$> (parseExp ExprT.Lit ExprT.Add ExprT.Mul s) 

{- 3 -}

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT.ExprT where
  lit n = ExprT.Lit n
  add a b = ExprT.Add a b
  mul a b = ExprT.Mul a b
  
{- 4 -}

instance Expr Integer where
  lit n = n
  add a b = (+) a b
  mul a b = (*) a b


instance Expr Bool where
  lit n = n > 0
  add a b = (||) a b
  mul a b = (&&) a b

newtype MinMax  = MinMax Integer deriving (Eq, Show)
newtype Mod7    = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit n = MinMax n
  add (MinMax a) (MinMax b) = MinMax $ min a b
  mul (MinMax a) (MinMax b) = MinMax $ max a b

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7
  
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool     = testExp :: Maybe Bool
testMM       = testExp :: Maybe MinMax
testSat      = testExp :: Maybe Mod7

{- 5: calculator -}

{-
data StackExp = PushI Integer
              | PushB Bool
              | Add
              | Mul
              | And
              | Or
              deriving Show
                       
type Program = [StackExp]
-}

instance Expr StackVM.Program where
  lit n = [StackVM.PushI n]
  add a b = a ++ b ++ [StackVM.Add]
  mul a b = a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe StackVM.Program
compile = parseExp lit add mul


{- 6: variables -}

newtype VarExprT = VarExprT Integer deriving (Eq, Show)

class HasVars a where
  var :: String -> a

instance Expr VarExprT where
  lit n = VarExprT n
  add (VarExprT a) (VarExprT b) = VarExprT $ (a + b)
  mul (VarExprT a) (VarExprT b) = VarExprT $ (a * b)

instance HasVars VarExprT where
  var _ = VarExprT 1 -- no storage, only constants


instance HasVars (M.Map String Integer -> Maybe Integer) where
  var s = (\m -> M.lookup s m) -- no storage, only constants

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer

withVars vs exp = exp $ M.fromList vs


instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n = \m -> Just n
  add fa fb = \m -> (+) <$> (fa m) <*> (fb m)
  mul fa fb = \m -> (*) <$> (fa m) <*> (fb m)

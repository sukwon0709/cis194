{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import qualified Data.Map as M
import           ExprT
import           Parser
import qualified StackVM  as VM


eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add e1 e2) = (eval e1) + (eval e2)
eval (Mul e1 e2) = (eval e1) * (eval e2)


evalStr :: String -> Maybe Integer
evalStr s = do
  expr <- parseExp Lit Add Mul s
  return $ eval expr


class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x = Lit x
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x | x <= 0 = False
        | otherwise = True
  add x y = x || y
  mul x y = x && y

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit x = MinMax x
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr VM.Program where
  lit x = [VM.PushI x]
  add x y = x ++ y ++ [VM.Add]
  mul x y = x ++ y ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile s = parseExp lit add mul s


class HasVar a where
  var :: String -> a

data VarExprT = VarLit Integer
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
              | VarVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit x = VarLit x
  add x y = VarAdd x y
  mul x y = VarMul x y

instance HasVar VarExprT where
  var x = VarVar x

instance HasVar (M.Map String Integer -> Maybe Integer) where
  var x = (\m -> M.lookup x m)

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x = (\m -> return x)
  add x y = (\m -> do
                xx <- x m
                yy <- y m
                return $ xx + yy)
  mul x y = (\m -> do
                xx <- x m
                yy <- y m
                return $ xx * yy)

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs exp = exp $ M.fromList vs

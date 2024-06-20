{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as S

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = Lit
    add = Add
    mul = Mul

instance Expr Integer where
    lit = id
    add = (+)
    mul = (*)

instance Expr Bool where
    lit = (0 <)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax a) (MinMax b) = MinMax(max a b)
    mul (MinMax a) (MinMax b) = MinMax(min a b)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit n = Mod7(mod n 7)
    add (Mod7 a) (Mod7 b) = Mod7(mod (a + b) 7)
    mul (Mod7 a) (Mod7 b) = Mod7(mod (a * b) 7)

reify :: ExprT -> ExprT
reify = id

eval :: ExprT -> Integer
eval (Lit int) = int
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
    Just expr -> Just (eval expr)
    Nothing -> Nothing

instance Expr S.Program where
    lit p = [S.PushI p]
    add p1 p2 = p1 ++ p2 ++ [S.Add]
    mul p1 p2 = p1 ++ p2 ++ [S.Mul]

compile :: String -> Maybe S.Program
compile = parseExp lit add mul
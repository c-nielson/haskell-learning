{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Party (main) where

import Employee
import Data.Tree
import Data.List ( sortOn )

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (empFun emp + fun)

instance Semigroup GuestList where
    (GL emps1 fun1) <> (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: b -> (a -> [b] -> b) -> Tree a -> b
treeFold x f (Node a []) = f a [x]
treeFold x f (Node a nodes) = f a (map (treeFold x f) nodes)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel emp [] = (GL [emp] (empFun emp), GL [] 0)
nextLevel emp list = (with, without) where
    with = foldr (\(_, without') acc -> acc <> without') (GL [emp] (empFun emp)) list
    without = foldr (\(with', without') acc -> acc <> moreFun with' without') (GL [] 0) list

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold (GL [] 0, GL [] 0) nextLevel

sortEmployees :: [Employee] -> [Employee]
sortEmployees = sortOn empName

format :: GuestList -> IO ()
format (GL l f) = putStrLn ("Total fun: " ++ show f) >> mapM_ (putStrLn . empName) (sortEmployees l)

main :: IO ()
main = readFile "company.txt" >>= (\n -> format $ maxFun (read n :: Tree Employee))
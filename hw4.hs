{-# OPTIONS_GHC -Wall #-}

import Data.List

fun1 :: [Integer] -> Integer
fun1  [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = foldr ((*) . (\x -> x - 2)) 1 . filter even

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

data Tree a = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert' Leaf
    where
        insert' x Leaf = Node 0 Leaf x Leaf
        insert' x (Node h l n r)
            | getHeight l > getHeight r = Node h l n (insert' x r)
            | getHeight l < getHeight r = Node h (insert' x l) n r
            | otherwise = Node (1 + max (getHeight l) (getHeight (insert' x l))) (insert' x l) n r
            where
                getHeight Leaf = -1
                getHeight (Node h' _ _ _) = h'

xor :: [Bool] -> Bool
xor = foldr (\old new -> (old || new) && not (old && new)) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram = map (\x -> 2*x + 1) . (\n -> [1..n] \\ [i + j + 2*i*j | i <- [1..n], j <- [1..n], 1 <= i && i <= j, i + j + 2*i*j <= n])

isPrime :: Integer -> Bool
isPrime n = n `elem` sieveSundaram (n `div` 2)
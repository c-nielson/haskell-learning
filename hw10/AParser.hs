{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative
import Control.Monad

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parser = Parser (\inString -> case runParser parser inString of
    Nothing -> Nothing
    Just (result, rest) -> Just (f result, rest))

instance Applicative Parser where
  pure f = Parser (\input -> Just (f, input))
  pa <*> pb = Parser (\input -> case runParser pa input of
    Nothing -> Nothing
    Just (f, rest) -> runParser (f <$> pb) rest)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$ char 'a' <*> char 'b'

intPair :: Parser (Integer, Integer)
intPair = selector <$> posInt <*> char ' ' <*> posInt where
  selector x y z = (x, z)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser f where f _ = Nothing
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) pa pb = Parser (\inputString -> runParser pa inputString <|> runParser pb inputString)

isUpper' :: Parser Char
isUpper' = satisfy isUpper

isUpper'_ :: Parser ()
isUpper'_ = void isUpper'

posInt_ :: Parser ()
posInt_ = void posInt

intOrUppercase :: Parser ()
-- intOrUppercase = void (satisfy isUpper) <|> void posInt
intOrUppercase = isUpper'_ <|> posInt_

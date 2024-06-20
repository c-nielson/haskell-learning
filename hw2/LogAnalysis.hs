{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage str = case words str of
    ("E":codeNum:ts:msg) ->
        if (read codeNum :: Int) > 0 && (read codeNum :: Int) < 101 then
            if (read ts :: Int) > 0 then
                LogMessage (Error (read codeNum)) (read ts) (unwords msg)
                else
                    Unknown "Invalid timestamp"
            else
                Unknown "Invalid error code"
    ("I":ts:msg) -> if (read ts :: Int) > 0 then
        LogMessage Info (read ts) (unwords msg)
        else
            Unknown "Invalid timestamp"
    ("W":ts:msg) -> if (read ts :: Int) > 0 then
        LogMessage Warning (read ts) (unwords msg)
        else
            Unknown "Invalid timestamp"
    _ -> Unknown "Invalid log format"

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg@(LogMessage _ ts _) (Node leftTree node@(LogMessage _ nodeTs _) rightTree) = if ts < nodeTs then
    Node (insert msg leftTree) node rightTree
    else
        Node leftTree node (insert msg rightTree)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = inOrder leftTree ++ [msg] ++ inOrder rightTree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong msgs = map getMsg $ inOrder . build $ filter important msgs
    where
        important (LogMessage (Error code) _ _) = code >= 50
        important _ = False
        getMsg (LogMessage _ _ msg) = msg
        getMsg _ = ""
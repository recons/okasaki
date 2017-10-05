module Ch2.Data.BinTree where

import Ch2.Data.Set

data BinTree a = Empty | Node (BinTree a) a (BinTree a)
  deriving (Show, Eq)

-- Реализация unordered set, данная в книге
instance Set BinTree where
  empty = Empty

  member _ Empty = False
  member x tree@(Node left y right)
    | x < y = member x left
    | x > y = member x right
    | otherwise = True

  insert x Empty = Node Empty x Empty
  insert x tree@(Node left y right)
    | x < y = Node right y (insert x left)
    | x > y = Node (insert x right) y left
    | otherwise = tree
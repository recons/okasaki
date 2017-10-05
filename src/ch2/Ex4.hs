module Ch2.Ex4 where

import Ch2.Data.Set
import Prelude

data BinTree a = Empty | Node (BinTree a) a (BinTree a)
    deriving (Show, Eq)

--  Совместите улучшения из предыдущих двух упражнений, и получите
--  версию insert, которая не делает ненужного копирования и
--  использует не более d+1 сравнений.
instance Set BinTree where
  empty = Empty

  member _ Empty = False
  member x tree@(Node _ y _) = go tree y
    where
      go Empty candidate = x == candidate
      go (Node left v right) candidate
        | x <= v = go left v
        | otherwise = go right candidate

  insert x Empty = Node Empty x Empty
  insert x tree@(Node _ y _) = go id tree y
    where
      go apply Empty candidate
        | x == candidate = tree
        | otherwise = apply $ Node Empty x Empty
      go apply (Node left v right) candidate
        | x <= v = go (\t -> apply $ Node t v right) left v
        | otherwise = go (\t -> apply $ Node left v t) right candidate
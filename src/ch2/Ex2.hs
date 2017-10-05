module Ch2.Ex2 where

import Ch2.Data.Set
import Prelude

data BinTree a = Empty | Node (BinTree a) a (BinTree a)
    deriving (Show, Eq)

-- В худшем случае member производит 2d сравнений, где
-- d - глубина дерева. Перепишите ее так, чтобы она делала не более
-- d + 1 сравнений, сохраняя элемент, который может оказаться
-- равным запрашиваемому (например, последний элемент, для которого
-- операция < вернула значение True или <= - False, и
-- производя проверку на равенство только по достижении дна дерева.
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
  insert x tree@(Node left y right)
    | x < y = Node right y (insert x left)
    | x > y = Node (insert x right) y left
    | otherwise = tree
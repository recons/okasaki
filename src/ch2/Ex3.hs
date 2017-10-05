module Ch2.Ex3 where

import Ch2.Data.Set
import Prelude

data BinTree a = Empty | Node (BinTree a) a (BinTree a)
    deriving (Show, Eq)

--  Вставка уже существующего элемента в двоичное дерево поиска копирует
--  весь путь поиска, хотя скопированные узлы неотличимы от
--  исходных. Перепишите insert так, чтобы она избегала
--  копирования с помощью исключений. Установите только один обработчик
--  исключений для всей операции поиска, а не по обработчику на итерацию.
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
  insert x tree@(Node _ y _) = go id tree
    where
      go apply Empty = apply $ Node Empty x Empty
      go apply (Node left v right)
        | x < v = go (\t -> apply $ Node t v right) left
        | x > v = go (\t -> apply $ Node left v t) right
        | otherwise = tree
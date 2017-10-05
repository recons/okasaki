module Ch2.Ex6 where

import Data.Maybe
import Prelude hiding (lookup)

import Ch2.Data.BinTree
import Ch2.Data.FiniteMap

--  Измените функтор UnbalancedSet так, чтобы он служил
--  реализацией не множеств, а конечных отображений(finite maps). На
--  2.10 приведена минимальная сигнатура для конечных
--  отображений. (Заметим, что исключение NotFound не
--  является встроенным в Стандартный ML - Вам придется его определить
--  самостоятельно. Это исключение можно было бы сделать частью
--  сигнатуры FiniteMap, чтобы каждая реализация
--  определяла собственное исключение NotFound, но удобнее,
--  если все конечные отображения будут использовать одно и то же
--  исключение.)

newtype BinTreeMap k v = Map { t :: BinTree (k, v) }
  deriving (Show, Eq)

instance FiniteMap BinTreeMap where
  empty = Map Empty

  bind k v (Map Empty) = Map (Node Empty (k, v) Empty)
  bind k v (Map tree@(Node topLeft (rootK, rootV) topRight)) = Map $ go id tree rootK
    where
      go apply Empty candidate
        | k == candidate && v == rootV = tree
        | k == candidate = Node topLeft (rootK, v) topRight
        | otherwise = apply $ Node Empty (k, v) Empty
      go apply (Node left (k', v') right) candidate
        | k <= k' = go (\t -> Node t (k', v') right) left k'
        | otherwise = go (\t -> Node left (k', v') t) right candidate

  lookup k (Map Empty) = Nothing
  lookup k (Map tree@(Node _ pair _)) = go tree pair
    where
      go Empty (k', v)
        | k == k' = Just v
        | otherwise = Nothing
      go (Node left pair'@(k', _) right) candidate
        | k <= k' = go left pair'
        | otherwise = go right candidate
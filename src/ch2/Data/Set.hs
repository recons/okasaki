module Ch2.Data.Set where

class Set f where
  empty :: f a
  member :: (Ord a) => a -> f a -> Bool
  insert :: (Ord a) => a -> f a -> f a
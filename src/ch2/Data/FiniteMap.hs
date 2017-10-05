module Ch2.Data.FiniteMap where

import Data.Maybe

class FiniteMap f where
  empty  :: f k v
  bind   :: (Ord k, Ord v) => k -> v -> f k v -> f k v
  lookup :: (Ord k) => k -> f k v -> Maybe v
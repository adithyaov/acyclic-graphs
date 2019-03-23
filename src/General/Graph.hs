{-# LANGUAGE TypeFamilies #-}

module General.Graph where

import Data.List (sort)
import Data.Set (Set, elems, filter, fromAscList, singleton, toList, union)
import qualified Data.Set as Set (empty)

-- Default class for a Graph as describes in the paper
class Graph g where
  type Vertex g
  empty :: g
  vertex :: Vertex g -> g
  overlay :: g -> g -> g
  connect :: g -> g -> g

data Relation a = R
  { domain :: Set a
  , relation :: Set (a, a)
  } deriving (Eq, Show, Ord)

instance Ord a => Graph (Relation a) where
  type Vertex (Relation a) = a
  empty = R Set.empty Set.empty
  vertex x = R (singleton x) Set.empty
  overlay x y = R (domain x `union` domain y) (relation x `union` relation y)
  connect x y =
    R
      (domain x `union` domain y)
      (relation x `union` relation y `union`
       fromAscList [(a, b) | a <- elems (domain x), b <- elems (domain y)])



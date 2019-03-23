{-# LANGUAGE TypeFamilies #-}

module General.Graph where

import Data.List (sort)
import qualified Data.Map as M
import Data.Set (Set, elems, filter, fromAscList, singleton, toList, union)
import qualified Data.Set as Set (empty)

-- Default class for a Graph as described in the paper
class Graph g where
  type Vertex g
  empty :: g
  vertex :: Vertex g -> g
  overlay :: g -> g -> g
  connect :: g -> g -> g
  adjMap :: g -> M.Map (Vertex g) [Vertex g]

-- Added functionality to get the basic information
-- from any Graph.
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
  adjMap g = foldr insert' defMap . toList . relation $ g
    where
      defMap = M.fromSet (const []) $ domain g
      insert' (x, y) = M.insertWith (++) x [y]

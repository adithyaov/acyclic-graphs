{-# LANGUAGE TypeFamilies #-}

module Prototype where

import Data.Set (Set, elems, filter, fromAscList, singleton, union)
import qualified Data.Set as Set (empty)
import Data.List (sort)

class Graph g where
  type Vertex g
  empty :: g
  vertex :: Vertex g -> g
  overlay :: g -> g -> g
  connect :: g -> g -> g

data Relation a = R
  { domain :: Set a
  , relation :: Set (a, a)
  } deriving (Eq, Show)

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

-- | newtype Acyclic, similar to the coding practice described in the Alga paper
newtype Acyclic a =
  A (Relation a) deriving (Show)

instance (Ord a) => Graph (Acyclic a) where
  type Vertex (Acyclic a) = a
  empty = A $ R Set.empty Set.empty
  vertex x = A $ R (singleton x) Set.empty
  overlay (A x) (A y) =
    A $ R (domain x `union` domain y) (relation x `union` relation y)
  connect (A x) (A y) =
    A $
    R
      (domain x `union` domain y)
      (relation x `union` relation y `union`
       fromAscList
         [(a, b) | a <- elems (domain x), b <- elems (domain y), b > a])

instance (Ord a) => Eq (Acyclic a) where
  (A g1) == (A g2) = vertexSet g1 == vertexSet g2 && edgeSet g1 == edgeSet g2
    where
      vertexSet = domain
      edgeSet = Data.Set.filter (\(a, b) -> b > a) . relation

-- | Simple Examples
e = empty :: Acyclic Int
v = vertex :: Int -> Acyclic Int
c = connect :: Acyclic Int -> Acyclic Int -> Acyclic Int
o = overlay :: Acyclic Int -> Acyclic Int -> Acyclic Int

allPossibleEdges :: [Int] -> Acyclic Int
allPossibleEdges = makeEdges . sort
  where
    makeEdges [] = empty
    makeEdges (x:xs) = o (foldr (o . c (v x) . v) e xs) $ makeEdges xs

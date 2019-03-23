{-# LANGUAGE TypeFamilies #-}

module Acyclic.Graph where

import General.Graph
import qualified Data.Graph as G
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Set (Set, elems, fromAscList, singleton, toList, union)
import qualified Data.Set as S

-- newtype Acyclic, similar to the coding practice described in the Alga paper
newtype Acyclic a =
  A (Relation a)
  deriving (Show)

instance (Ord a) => Graph (Acyclic a) where
  type Vertex (Acyclic a) = a
  empty = A empty
  vertex = A . vertex
  overlay (A x) (A y) =
    A $ overlay x y
  connect (A x) (A y) =
    A $ R (domain genRes) newRelation
      where
        genRes = connect x y
        newRelation = S.filter (uncurry (>)) $ relation genRes
  adjMap (A g) = adjMap g

instance (Ord a) => Eq (Acyclic a) where
  (A g1) == (A g2) = vertexSet g1 == vertexSet g2 && edgeSet g1 == edgeSet g2
    where
      vertexSet = domain
      edgeSet = S.filter (uncurry (>)) . relation


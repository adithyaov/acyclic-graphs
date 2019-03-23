{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Acyclic.Graph where

import Acyclic.Util
import qualified Data.Graph as G
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Set (Set, elems, fromAscList, singleton, toList, union)
import qualified Data.Set as S
import General.Graph

-- newtype Acyclic, similar to the coding practice described in the Alga paper
newtype AcyclicRelation a =
  A (Relation a)
  deriving (Show)

instance (Ord a) => Graph (AcyclicRelation a) where
  type Vertex (Acyclic a) = a
  empty = A empty
  vertex = A . vertex
  overlay (A x) (A y) = A $ overlay x y
  connect (A x) (A y) = A $ R (domain genRes) newRelation
    where
      genRes = connect x y
      newRelation = S.filter (uncurry (>)) $ relation genRes
  adjMap (A g) = adjMap g

-- This is not really required as we are filtering the
-- improper vertices during the connect operation.
instance (Ord a) => Eq (AcyclicRelation a) where
  (A g1) == (A g2) = vertexSet g1 == vertexSet g2 && edgeSet g1 == edgeSet g2
    where
      vertexSet = domain
      edgeSet = S.filter (uncurry (>)) . relation

-- Important function to glance upon, The result of `scc` should
-- unsafely convert the result to Acyclic. Whenever a graph results
-- in an acyclic computation the graph should be converted to the
-- type Acyclic
unsafeConvertToAcyclic ::
     (Vertex b ~ SimpleOrder (Vertex g), Ord (Vertex g), Graph g, Graph b)
  => g
  -> b
unsafeConvertToAcyclic g = foldr overlay empty flattened
  where
    (g', f') =
      G.graphFromEdges' . map (\(x, y) -> ((), x, y)) . M.toList . adjMap $ g
    ordered = map ((\(_, x, y) -> (x, y)) . f') $ G.topSort g'
    indexMap = M.fromList $ zip (map fst ordered) (makeAsc $ length ordered)
    vertex' v = vertex $ SimpleOrder (indexMap M.! v, v)
    flattened =
      map
        (\(x, y) -> connect (vertex' x) (foldr (overlay . vertex') empty y))
        ordered

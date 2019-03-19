{-# LANGUAGE TypeFamilies #-}

module Prototype where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Set (Set, elems, filter, fromAscList, singleton, toList, union)
import qualified Data.Set as Set (empty)

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

-- | newtype Acyclic, similar to the coding practice described in the Alga paper
newtype Acyclic a =
  A (Relation a)
  deriving (Show)

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

-- | Simple example of file dependency
graph :: Relation String
graph =
  overlay
    (overlay
       (overlay
          (connect (vertex "A.hs") (vertex "B.hs"))
          (connect (vertex "B.hs") (vertex "D.hs")))
       (connect (vertex "D.hs") (vertex "B.hs")))
    (connect (vertex "D.hs") (vertex "C.hs"))

circut [] = empty
circut (x:xs) = foldr overlay empty $ zipWith connect (x : xs) (xs ++ [x])

-- | unsafeTopSortRes is the result of applying unsafeTopSort on scc_ giving us topological ordering of the vertices. It is unsafe.
unsafeTopSortRes =
  [vertex "A.hs", circut [vertex "B.hs", vertex "D.hs"], vertex "C.hs"] :: [Relation String]

-- | Every graph should have a way to extract the edgeSet
edgeSet =
  [ (vertex "A.hs", circut [vertex "B.hs", vertex "D.hs"])
  , (circut [vertex "B.hs", vertex "D.hs"], vertex "C.hs")
  ] :: [(Relation String, Relation String)]

newtype SimpleOrder a =
  SimpleOrder (Int, a)
  deriving (Eq, Show)

instance (Eq a) => Ord (SimpleOrder a) where
  compare (SimpleOrder (x, _)) (SimpleOrder (y, _)) = compare x y

makeAsc :: Int -> [Int]
makeAsc n = reverse $ mA n
  where
    mA 0 = []
    mA k = k : mA (k - 1)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

scc :: Acyclic (SimpleOrder (Relation String))
scc = overlay vertices edges
  where
    vList = zip (makeAsc $ length unsafeTopSortRes) unsafeTopSortRes
    vLevels = Map.fromList $ map (\(a, b) -> (b, a)) vList
    vertices = foldr (overlay . vertex . SimpleOrder) empty vList
    oriEdgeMapF = mapTuple (\x -> vertex . SimpleOrder $ (vLevels Map.! x, x))
    edges = foldr (overlay . uncurry connect . oriEdgeMapF) empty edgeSet

module Prototype where

import Acyclic.Graph
import Acyclic.Util
import General.Graph

-- | Simple example of file dependency
graph :: Relation (Relation String)
graph =
  overlay
    (connect (vertex $ vertex "A.hs") (vertex $ circut [vertex "B.hs", vertex "D.hs"]))
    (connect (vertex $ circut [vertex "B.hs", vertex "D.hs"]) (vertex $ vertex "D.hs"))

circut [] = empty
circut (x:xs) = foldr overlay empty $ zipWith connect (x : xs) (xs ++ [x])

acyclicGraph :: AcyclicRelation (SimpleOrder (Relation String))
acyclicGraph = unsafeConvertToAcyclic graph

-- The functions which result in an acyclic graph should
-- be described like such,
-- Assuming the type signature of scc is as follows,
-- scc :: Relation a -> Relation b
-- scc' can be implemented like follows,
-- scc' = unsafeConvertToAcyclic . scc
-- Then the type signature of scc' would be like,
-- scc' :: Relation a -> AcyclicRelation (SimpleOrder b)




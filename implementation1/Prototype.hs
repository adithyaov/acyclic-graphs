module Prototype where

import Acyclic.Graph
import Acyclic.Util
import General.Graph

connect' x y = connect (vertex x) (vertex y)

-- | Simple example of file dependency
graph :: Relation Int
graph = overlay (overlay (connect' 1 2) (connect' 2 3)) (connect' 3 1)


acyclicGraph :: AcyclicRelation (SimpleOrder Int)
acyclicGraph = unsafeConvertToAcyclic graph

-- The functions which result in an acyclic graph should
-- be described like such,
-- Assuming the type signature of scc is as follows,
-- scc :: Relation a -> Relation b
-- scc' can be implemented like follows,
-- scc' = unsafeConvertToAcyclic . scc
-- Then the type signature of scc' would be like,
-- scc' :: Relation a -> AcyclicRelation (SimpleOrder b)




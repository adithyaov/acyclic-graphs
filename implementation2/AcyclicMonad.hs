module AcyclicMonad where

import Control.Monad.Trans.State.Strict

type Vertex = Int

-- eg. Edges 4 [2, 3] (Edges 3 [1] (Edges 2 [1] (Edges 1 [] Nil)))
-- represents 4 * 2 + 4 * 3 + 3 * 1 + 2 * 1 + 1
data DAG = DAG Vertex [Vertex] DAG | Nil deriving (Show)

vertex (DAG i _ _) = i
vertex Nil = 0

addSingleton s = DAG (1 + vertex s) [] s
addEdges es s = DAG (1 + vertex s) (map vertex es) s

singleton = modify addSingleton >> get

edgeTo es = modify (addEdges es) >> get

dag = snd . flip runState Nil

dagTest = dag $ do
  v1 <- singleton
  v2 <- singleton
  edgeTo [v1, v2]
  





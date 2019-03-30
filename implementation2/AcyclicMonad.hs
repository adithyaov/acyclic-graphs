module AcyclicMonad where

import Control.Monad.Trans.State.Strict

type Vertex = Int

-- eg. Edges 4 [2, 3] (Edges 3 [1] (Edges 2 [1] (Edges 1 [] Nil)))
-- represents 4 * 2 + 4 * 3 + 3 * 1 + 2 * 1 + 1
data DAG
  = Cons Vertex
         [DAG]
         DAG
  | Nil
  deriving (Show)

-- A simple helper function
vertex (Cons i _ _) = i
vertex Nil = 0

-- A State monad creating a singleton
singleton = modify (\s -> Cons (1 + vertex s) [] s) >> get

-- A State monad resulting in proper edges
edgeTo es = modify (\s -> Cons (1 + vertex s) es s) >> get

-- A simple function to run the state to get DAG in return
dag = snd . flip runState Nil

-- The result : DAG 3 [1..DAG,2..DAG] (DAG 2 [] (DAG 1 [] Nil))
dagTest =
  dag $ do
    v1 <- singleton
    v2 <- singleton
    edgeTo [v1, v2]

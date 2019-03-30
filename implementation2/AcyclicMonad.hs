module AcyclicMonad where

import Control.Monad.Trans.State.Strict

type Vertex = Int

-- eg. Edges 4 [2, 3] (Edges 3 [1] (Edges 2 [1] (Edges 1 [] Nil)))
-- represents 4 * 2 + 4 * 3 + 3 * 1 + 2 * 1 + 1
data DAG a
  = Cons a
         [DAG a]
         (DAG a)
  | Nil
  deriving (Show)

-- A State monad creating a singleton
singleton a = modify (Cons a []) >> get

-- A State monad resulting in proper edges
edgeTo a es = modify (Cons a es) >> get

-- A simple function to run the state to get DAG in return
dag = snd . flip runState Nil

-- The result : DAG 3 [1,2] (DAG 2 [] (DAG 1 [] Nil))
dagTest =
  dag $ do
    v1 <- singleton 1
    v2 <- singleton 2
    3 `edgeTo` [v1, v2]


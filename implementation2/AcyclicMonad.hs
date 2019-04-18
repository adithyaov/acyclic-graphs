module AcyclicMonad
  ( singleton
  , edgeTo
  , dag
  , dagTest
  ) where

import Control.Monad.Trans.State.Strict

type Vertex = Int

newtype DAG = DAG DAG' deriving Show

-- eg. Edges 4 [2, 3] (Edges 3 [1] (Edges 2 [1] (Edges 1 [] Nil)))
-- represents 4 * 2 + 4 * 3 + 3 * 1 + 2 * 1 + 1
data DAG'
  = Cons Vertex
         [DAG']
         DAG'
  | Nil
  deriving (Show)

-- A simple helper function
vertex (Cons i _ _) = i
vertex Nil = 0

-- A State monad creating a singleton
singleton :: State DAG' DAG'
singleton = modify (\s -> Cons (1 + vertex s) [] s) >> get

-- A State monad resulting in proper edges
edgeTo :: [DAG'] -> State DAG' DAG'
edgeTo es = modify (\s -> Cons (1 + vertex s) es s) >> get

-- A simple function to run the state to get DAG in return
dag = DAG . snd . flip runState Nil

dagTest = do
    v1 <- singleton
    v2 <- singleton
    edgeTo [v1, v2]
    singleton
    





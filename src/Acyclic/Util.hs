module Acyclic.Util where

-- A simple order used for the underlying order
-- when any graph of type General.Relation is
-- converted to Acyclic.Graph
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

module Graph
( Map.empty
, connect
, connectBidirectional
, adjacentTo
, fromList'
, fromList
, Graph)
where

import qualified Data.Map as Map

type Graph a = Map.Map a [a]

connect :: (Ord a) => a -> a -> Graph a -> Graph a
connect u v g = case (Map.lookup u g) of
    Nothing -> Map.insert u [v] g
    Just x  -> Map.insert u (v:x) g

connectBidirectional :: (Ord a) => a -> a -> Graph a -> Graph a
connectBidirectional u v = connect v u . connect u v

adjacentTo :: (Ord a) => a -> Graph a -> [a]
adjacentTo = Map.findWithDefault []

fromList' :: (Ord a) => (a -> a -> Graph a -> Graph a) -> [(a, a)] -> Graph a
fromList' f = foldr connectTuple Map.empty
    where
        connectTuple (x, y) = f x y

fromList :: (Ord a) => [(a, a)] -> Graph a
fromList = fromList' connect

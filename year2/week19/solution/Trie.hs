module Trie
( Trie(..)
, empty
, insert
, member
, fromList
, leaf
, filter
, sequenceEnd)
where

import qualified Data.Map as Map
import Prelude hiding (filter)

-- | Trie consists of a flag denoting if it is a word end or not
--   and a map of labeled outgoing edges
data Trie a = Trie Bool (Map.Map a (Trie a)) deriving (Show)

-- | Construct an empty trie
empty :: Trie a
empty = Trie False Map.empty

-- | Checks if the given node denotes the end of a sequence
sequenceEnd :: Trie a -> Bool
sequenceEnd (Trie e _) = e

-- | Check if a given trie node is a leaf
leaf :: Trie a -> Bool
leaf (Trie _ m) = Map.null m

-- | Filter the child nodes on a given predicate
filter :: (Trie a -> Bool) -> Trie a -> Trie a
filter p (Trie e m) = Trie e (Map.filter p m)

-- | Insert a sequence into the trie
insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie _ m)     = Trie True m
insert (x:xs) (Trie e m) = case Map.lookup x m of
    Nothing -> Trie e (Map.insert x (insert xs empty) m)
    Just n  -> Trie e (Map.insert x (insert xs n) m)

-- | Check if a sequence is a member of the trie
member :: Ord a => [a] -> Trie a -> Bool
member [] (Trie e _)     = e
member (x:xs) (Trie _ m) = case Map.lookup x m of
    Nothing -> False
    Just n  -> member xs n

-- | Create a trie from a list of sequences
fromList :: Ord a => [[a]] -> Trie a
fromList = foldr insert empty

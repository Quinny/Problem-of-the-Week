module Week1 where

import Data.Bits (xor, popCount)
import Data.List.Split (splitOn)
import Control.Arrow

type Rating = Int
type Name = String

data Person = Person Name Rating deriving (Show, Eq)
data Pairing = Pairing Person Person Rating deriving (Eq)


instance Ord Pairing where
    (Pairing _ _ r1) `compare` (Pairing _ _ r2) = r1 `compare` r2

instance Show Pairing where
    show (Pairing p1 p2 _) = (name p1) ++ " " ++ (name p2)

name :: Person -> String
name (Person name _) = name

parsePerson :: String -> Person
parsePerson s = Person (tokens !! 0) (read (tokens !! 1))
    where
        tokens = splitOn " " s

fromCouple :: Person -> Person -> Pairing
fromCouple p1 p2 = Pairing p1 p2 (compatibility p1 p2)

parse :: String -> [Person]
parse s = map parsePerson (tail $ lines s)

compatibility :: Person -> Person -> Rating
compatibility (Person _ r1) (Person _ r2) = popCount $ xor r1 r2

pairings :: [Person] -> [Pairing]
pairings people = [fromCouple x y | x <- people, y <- people,  x /= y]

main = do
    input <- parse <$> getContents
    print $ maximum $ pairings $ input
    print $ minimum $ pairings $ input

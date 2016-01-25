import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Control.Arrow

type TileMap = Map.Map Int Int
type Portal = (Int, Int)

-- | Parses a string of the form "n m" into a tuple of (Int, Int)
--   Note that the definition of &&& is ROUGHLY
--   (&&&) :: (a -> b) -> (a -> c) -> a -> (b, c)
--   (&&&) f g x = (f x, g x)
--   That is, it feeds the input to both arguments and collects the results
parsePortal :: String -> Portal
parsePortal = ((read . (!! 0)) &&& (read . (!! 1))) . splitOn " "

-- | Parses the input into a TileMap
parse :: String -> TileMap
parse = Map.fromList . map parsePortal . tail . lines

-- | Given the tile map, the current tile, and the roll
--   determines where the player will end
endPoint :: TileMap -> Int -> Int -> Int
endPoint tiles tile roll = case Map.lookup (tile + roll) tiles of
    Nothing -> tile + roll
    Just x  -> x

-- | Generic search space expansion algorithm.
search :: c -> (c -> [c]) -> (c -> Bool) -> c
search start expand done = search' [start]
    where
        search' (x:xs)
            | done x = x
            | otherwise = search' (xs ++ expand x)

-- | Given the position of all snakes and ladders and a position on the board
--   generate all possible ending locations assuming a six sided die
nextMoves :: TileMap -> (Int, Int) -> [(Int, Int)]
nextMoves tiles (tile, move) = markMove $ map (endPoint tiles tile) [1..6]
    where
        markMove xs = zip xs $ repeat (move + 1)

-- | The game is won once tile 100 is reached
gameWon :: (Int, Int) -> Bool
gameWon (tile, _) = tile == 100

minimumMoves :: TileMap -> Int
minimumMoves tiles = snd $ search (1, 0) (nextMoves tiles) gameWon

main = (minimumMoves . parse) <$> getContents >>= print

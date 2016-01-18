import Data.MemoCombinators

-- | The Prelude interact function only accepts a function
--   which returns a String, this version accepts a function which returns
--   any value that can be represented with a string (part of the Show typeclass)
interact' :: (Show a) => (String -> a) -> IO ()
interact' f = f <$> getContents >>= print

-- | Parse the input, ignore the dimensions as we can just take the length
parse :: String -> [[Int]]
parse input = tail $ map ((map read) . words) (lines input)

-- | Find the maximum path sum
pathSum :: [[Int]] -> Int
pathSum shelf = loop (rows - 1) (cols - 1)
    where
        -- | Memoization combinator, abstracts away the DP
        loopMemo = memo2 integral integral loop
        rows = length shelf
        cols = length (head shelf)
        inf = 9999999999999
        loop i j
            | i == 0 && j == 0   = shelf !! 0 !! 0
            | i < 0 || i >= rows = (-inf)
            | j < 0 || j >= cols = (-inf)
            | otherwise =
                (shelf !! i !! j) + max (loopMemo (i - 1) j) (loopMemo i (j - 1))

main = interact' (pathSum . parse)

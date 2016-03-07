majorityCandidate :: (Eq a) => [a] -> a
majorityCandidate (x:xs) = fst $ foldr majorityCandidate' (x, 1) xs
    where
        majorityCandidate' y (x, c)
            | c == 0    = (y, 1)
            | x == y    = (x, c + 1)
            | otherwise = (x, c - 1)

isMajority :: (Eq a) => a -> [a] -> Bool
isMajority x xs = count x xs > length xs `div` 2
    where count x = length . filter (==x)

majorityElement :: (Eq a) => [a] -> Maybe a
majorityElement xs
    | isMajority x xs = Just x
    | otherwise       = Nothing
        where x = majorityCandidate xs

main = lines <$> getContents >>= output . majorityElement
    where
        output (Just x) = putStrLn x
        output Nothing  = putStrLn "no majority"

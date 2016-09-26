import Data.List (partition, sort)
import Data.List.Split (splitOn)
import Control.Monad (replicateM)
import System.IO

-- | A lady is defined by her X, Y coordinates and her distance from William.
data Lady = Lady Int Int Int

-- | Equality and comparison are based only on distance from William.
instance Eq Lady where
  (==) (Lady _ _ d1) (Lady _ _ d2) = d1 == d2

instance Ord Lady where
  compare (Lady _ _ d1) (Lady _ _ d2) = d1 `compare` d2

instance Show Lady where
  show (Lady x y _) = show x ++ " " ++ show y

-- | The squared distance between two x y coordinates.  The square distance
--   is used because for this particular problem only the relative ordering
--   between distances is needed.  This allows for the expensive call to sqrt
--   to be skipped.
distanceSq :: (Int, Int) -> (Int, Int) -> Int
distanceSq (x1, y1) (x2, y2) = (x1 - x2)^2 + (y1 - y2)^2

-- | Given a ladies X, Y coordinates and Williams X,Y coordinates, create
--   a Lady.
toLady :: (Int, Int) -> (Int, Int) -> Lady
toLady willPos ladyPos = Lady (fst ladyPos) (snd ladyPos) (distanceSq willPos ladyPos)

-- | Select the kth smallest element from a list using the < operator for
--   comparison.
quickselect :: (Ord a) => Int -> [a] -> a
quickselect k (x:xs)
  | k < lengthLeft = quickselect k left
  | k > lengthLeft = quickselect (k - lengthLeft - 1) right
  | otherwise = x
    where (left, right) = partition (< x) xs
          lengthLeft = length left

-- | Return the k smallest elements in a list with respect to the < operator.
--   The resulting list may not be sorted.
smallestK :: (Ord a) => Int -> [a] -> [a]
smallestK k original = take k $ filter (<= e) original
  where e = quickselect (k-1) original

-- | Helper function for reading a point (pair of ints) from stdin.
readPoint :: IO (Int, Int)
readPoint = do
  [x, y] <- map read . splitOn " " <$> getLine
  return (x, y)

-- | Helper function for reading an int from stdin.
readInt :: IO Int
readInt = read <$> getLine

main = do
  william <- readPoint
  k <- readInt
  n <- readInt
  ladies <- map (toLady william) <$> replicateM n readPoint
  mapM_ print $ sort $ smallestK k ladies

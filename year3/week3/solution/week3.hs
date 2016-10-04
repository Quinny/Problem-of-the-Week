import qualified Data.Map as Map
import Data.Function (on)
import Control.Arrow (second)
import Control.Monad (replicateM, mapM_)

-- | Helper type aliases
type Histogram a = Map.Map a Int
type Term        = String
type Document a  = (a, Histogram Term)
type Corpus a    = [Document a]

-- | Helper for doing floating point division on two integers.
floatDivide :: Int -> Int -> Double
floatDivide = (/) `on` fromIntegral

-- | Converts a list of elements (which may have repeats) to a histogram
--   representing the frequency of each element.
toHistogram :: (Ord a) => [a] -> Histogram a
toHistogram = foldr f Map.empty
  where f = flip (Map.insertWith (+)) 1

-- | Get the frequency of a term in a document.  If the term is not present
--   in the document, 0 will be returned.
tf :: Term -> Document a -> Int
tf term (label, doc) = Map.findWithDefault 0 term doc

-- | Compute the IDF factor for a term in a corpus.
idf :: Term -> Corpus a -> Double
idf term corpus
  | documentsContainingTerm == 0 = 0 -- Don't divide by 0.
  | otherwise = logBase 10 (totalDocuments `floatDivide` documentsContainingTerm)
    where
      totalDocuments          = length corpus
      documentsContainingTerm = length $ filter ((>0) . tf term) corpus

-- | Returns a list of (document label, tfidf score) pairs for each document in
--   the corpus given the query term.
tfidf :: Term -> Corpus a -> [(a, Double)]
tfidf term corpus = map (second (*idfFactor)) termFreqencies
  where
    termFreqencies = zip (map fst corpus) $ map (fromIntegral . tf term) corpus
    idfFactor = idf term corpus

main = do
  numberOfDocuments <- read <$> getLine
  corpus <- zip [1..] <$> replicateM numberOfDocuments (toHistogram <$> words <$> getLine)
  queryTerm <- getLine
  mapM_ (putStrLn . format) $ tfidf queryTerm corpus
    where format (x, y) = show x ++ " " ++ show y

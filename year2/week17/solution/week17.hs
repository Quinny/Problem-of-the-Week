import qualified Data.Map as Map
import Data.Function (on)
import Data.List (sortBy, insertBy)
import Control.Arrow (second)

type Histogram a = [(a, Int)]
data HuffmanTree a = Leaf a Int
    | Branch (HuffmanTree a) (HuffmanTree a) Int

-- | Given a huffman node, return is frequency
frequency :: HuffmanTree a -> Int
frequency (Leaf _ w)     = w
frequency (Branch _ _ w) = w

-- | Combine two trees into one branch, sum their frequencies
combine :: HuffmanTree a -> HuffmanTree a -> HuffmanTree a
combine n m = Branch n m (frequency n + frequency m)

-- | Generate a histogram from a list
fromList :: (Ord a) => [a] -> Histogram a
fromList = Map.toList . foldr f Map.empty
    where
        f = flip (Map.insertWith (+)) 1

-- | Build a huffman tree from a histogram
buildTree :: Histogram a -> HuffmanTree a
buildTree = build . map (uncurry Leaf) . sortBy (compare `on` snd)
    where
        build [x] = x
        build (x:y:xs) = build $ insertBy (compare `on` frequency) (combine x y) xs

-- | Given a huffman tree, generate the encodings
coding :: HuffmanTree a -> [(a, String)]
coding (Leaf v _)     = [(v, [])]
coding (Branch l r _) = map (prepend '0') (coding l) ++ map (prepend '1') (coding r)
    where
        -- | second f (x, y) = (x, f y)
        prepend x = second (x:)

main = (coding . buildTree . fromList . words) <$> getContents >>= print

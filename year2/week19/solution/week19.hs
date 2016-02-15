import qualified Trie as Trie

-- | Determines validity of a huffman encoding
--   If a non-leaf word end exists in the trie,
--   then one code must be a prefix of another
validEncoding :: Trie.Trie a -> Bool
validEncoding t
    | Trie.sequenceEnd t = Trie.leaf t
    | otherwise          = Trie.leaf (Trie.filter (not . validEncoding) t)

main = interact $ show . validEncoding . Trie.fromList . lines

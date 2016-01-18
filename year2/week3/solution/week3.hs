import Control.Monad (replicateM)
import qualified Data.Set as Set

type Dictionary = Set.Set String
type Sentence = String

isValid :: Dictionary -> Sentence -> Bool
isValid dict s = isValid' dict "" s
    where
        isValid' :: Dictionary -> String -> Sentence -> Bool
        isValid' dict prefix [] = Set.member prefix dict
        isValid' dict prefix rest@(x:xs)
            | Set.member prefix dict = isValid' dict (prefix ++ [x]) xs
                || isValid' dict [] rest
            | otherwise = isValid' dict (prefix ++ [x]) xs

boolToInt True  = 1
boolToInt False = 0

main = do
    dictSize <- read <$> getLine
    dict     <- Set.fromList <$> replicateM dictSize getLine

    nSentences <- read <$> getLine
    sentences  <- replicateM nSentences getLine

    mapM_ (print . boolToInt . isValid dict) sentences

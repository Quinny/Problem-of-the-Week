import Data.Bits
import Data.List

getOctets :: Int -> [Int]
getOctets ip = map mask [3,2,1,0]
  where mask x = (shift ip $ (-8) * x) .&. 255

showIP :: Int -> String
showIP = intercalate "." . map show . getOctets

main = showIP <$> read <$> getLine >>= print

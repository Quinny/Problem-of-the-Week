import Data.List.Split (splitOn)
import Control.Monad (replicateM)
import Control.Arrow (second)

-- | A data type which models a perceptron layer.
data PerceptronLayer = PerceptronLayer {
  weights            :: [Double],
  bias               :: Double,
  learningRate       :: Double,
  activationFunction :: (Double -> Double)
}

-- | Creates an empty perceptron layer with the supplied learning rate and
--   activation function.  Weights and bias are initialized to 0.
emptyLayer :: Int -> Double -> (Double -> Double) -> PerceptronLayer
emptyLayer nInputs rate af = PerceptronLayer ws 0 rate af
  where ws = replicate nInputs 0

-- | Applies the perceptron to the feature vector and returns the output.
apply :: PerceptronLayer -> [Double] -> Double
apply layer features = activationFunction layer $ (dot (weights layer) features) + bias layer
  where dot xs ys = sum $ zipWith (*) xs ys

-- | Given a feature vector and an expected output, update the weights and bias
--   of the layer according to the perceptron learning rule, and return the error.
learn :: PerceptronLayer -> [Double] -> Double -> (PerceptronLayer, Double)
learn layer features trueOutput = (PerceptronLayer nws nb (learningRate layer) (activationFunction layer), error)
  where
    error = trueOutput - (apply layer features)
    delta (weight, feature) = weight + ((learningRate layer) * feature * error)
    nws = map delta $ zip (weights layer) features
    -- | The bias can be treated as a weight with a constant input feature of 1.
    nb = delta (bias layer, 1)

-- | Continuously train on the dataset until the error is 0.
trainUntillConvergence :: PerceptronLayer -> [([Double], Double)] -> PerceptronLayer
trainUntillConvergence layer dataset
  | error == 0 = layer
  | otherwise = trainUntillConvergence layer' dataset
    where
      (layer', error) = foldl update (layer, 0) dataset
      update (model, error) (features, label) = second (+error) $ learn model features label

-- | The step activation function.
step :: Double -> Double
step x = if x > 0 then 1 else -1

-- | Read a labeled example from stdin.
readLabeledExample :: IO ([Double], Double)
readLabeledExample = do
  [x, y, label] <- map read . splitOn " " <$> getLine
  return ([x, y], label)

-- | Read an unlabled example from stdin.
readUnlabeledExample :: IO [Double]
readUnlabeledExample = map read . splitOn " " <$> getLine

-- | Read an integer from stdin.
readInt :: IO Int
readInt = read <$> getLine

main = do
  nTrainingSamples <- readInt
  trainingData <- replicateM nTrainingSamples readLabeledExample
  let model = trainUntillConvergence (emptyLayer 2 0.01 step) trainingData 

  nTestingSamples <- readInt
  testingData <- replicateM nTestingSamples readUnlabeledExample
  mapM_ putStrLn (map toLabel $ map (apply model) testingData)
    where toLabel x = if x > 0 then "Cool" else "Nerd"

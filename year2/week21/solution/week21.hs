import System.Random
import Data.List (maximumBy)
import Data.Function (on)
import Control.Monad.Random
import Control.Monad.State

-- | Pseudo genetic algorithm to evolve one string into another
--   and show all stages

-- | Rate at which we change a single character
mutationRate :: Int
mutationRate = 30

-- | Function to evaluate fitness of a string,
--   number of differing characters between the target
hammingDistance :: String -> String -> Int
hammingDistance s1 s2 = length $ filter (uncurry (==)) $ zip s1 s2

-- | Generate a random string of length n
randstr :: Int -> StdGen -> String
randstr n seed = take n $ randomRs ('A', 'z') seed

-- | randomR that operates in the state monad.
--   Now, whenever we have a StdGen in a state monad we can just call this
--   to get a new random value. It handled threading the seed through everything.
sRandomR :: (RandomGen s, Random b, MonadState s m) => (b, b) -> m b
sRandomR bounds@(l,h) = do
    seed <- get
    let (val, seed') = randomR bounds seed
    put seed'
    return val

-- | Return a new character with a given probability rate, otherwise return
--   the input character
maybeChange :: Int -> Char -> State StdGen Char
maybeChange rate c = do
    val <- sRandomR (1, 100)
    if val < rate
        then sRandomR ('A', 'z')
        else return c

-- | Mutate a string by maybe changing each of its characters
mutate :: Int -> String -> State StdGen String
mutate rate = mapM (maybeChange rate)

-- | Evolve one string into another and return all intermediate changes
evolve :: String  -- ^ Current.
       -> String  -- ^ Target.
       -> (String -> State StdGen String) -- ^ Mutation function.
       -> (String -> Int)  -- ^ Fitness function.
       -> State StdGen [String]
evolve initial target mutationFn fitnessFn = evolveImpl initial []
  where evolveImpl current stages
            | current == target = return $ reverse (target:stages)
            | otherwise = do
                  nextGeneration <- replicateM 100 (mutationFn current)
                  let best = maximumBy (compare `on` fitnessFn) nextGeneration
                  evolveImpl best (best:stages)

main = do
    target <- getLine
    initialStrSeed <- newStdGen
    let initial = randstr (length target) initialStrSeed
    evolutionSeed <- newStdGen
    mapM_ print $ evalState (evolve initial target (mutate 30) (hammingDistance target)) evolutionSeed
    putStrLn initial

import Data.List (isPrefixOf)
import Network (connectTo, PortID(PortNumber))
import System.IO (hGetLine, hPutStrLn, Handle)

adapt :: (Int, Int) -> String -> (Int, Int)
adapt (low, high) response
    | response == "low"  = (mid + 1, high)
    | response == "high" = (low, mid)
        where mid = (high + low) `div` 2

handleResponse :: Handle -> (Int, Int) -> String -> IO ()
handleResponse h bounds response
    | "good" `isPrefixOf` response = putStrLn response
    | otherwise                    = playGame h (adapt bounds response)

playGame :: Handle -> (Int, Int) -> IO ()
playGame h bounds@(low, high) = do
    hPutStrLn h (show guess)
    hGetLine h >>= handleResponse h bounds
        where guess = (high + low) `div` 2

main = do
    handle <- connectTo "api.quinnftw.com" (PortNumber 9000)
    playGame handle (1, 1000)

import qualified Graph as G
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Arrow

type Name = String
type Friendship = (Name, Name)
type Level = Int

parse :: String -> G.Graph Name
parse = G.fromList' G.connectBidirectional . map parseFriendship . tail . lines

parseFriendship :: String -> Friendship
parseFriendship = ((!! 0) &&& (!! 1)) . splitOn " "

markParent :: Name -> [Name] -> [Friendship]
markParent x xs = zipWith (,) xs (repeat x)

solution :: G.Graph Name -> M.Map Name Level
solution g = solution' g (M.insert "Quinn" 0 M.empty) (markParent "Quinn" $ G.adjacentTo "Quinn" g)
    where
        solution' :: G.Graph Name -> M.Map Name Level -> [Friendship] -> M.Map Name Level
        solution' _ visited [] = visited
        solution' g visited ((child, parent):xs) =
                solution' g (M.insert child nextLevel visited) (xs ++ mustVisit)
                    where
                        removeVisited = filter (flip (M.notMember) visited)
                        nextLevel = (visited M.! parent) + 1
                        mustVisit = markParent child $ removeVisited (G.adjacentTo child g)

main = do
    input <- getContents
    mapM_ print (M.toList $ solution $ parse input)

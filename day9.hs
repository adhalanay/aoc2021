import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.List.Split
import Data.List (sort)

rowLength :: [[a]] -> Int
rowLength = length . head

point :: [[Int]] -> (Int, Int) -> Int
point m (x,y) = if 0<= y && y < length m && 0 <= x && x < rowLength m
                   then m !! y !! x
                   else 9
neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (x,y) =[(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

isLocalMinimum :: (Int,Int) -> [[Int]]->Bool
isLocalMinimum p@(x,y) m = all ((> point m p) . point m) (neighbours p)

localMinima ::(Int,Int) -> [[Int]] -> [(Int,Int)]
localMinima p@(x,y) m
  | x < rowLength m = if isLocalMinimum p m
                      then p:localMinima (x+1,y) m
                      else localMinima (x+1,y) m
  | y < length m = localMinima (0,y+1) m
  | otherwise = []

riskLevel :: [[Int]] -> (Int,Int) -> Int
riskLevel m p = 1 + point m p

basin :: [[Int]]->[(Int,Int)]->(Int,Int) -> [(Int,Int)]
basin m s p@(x,y) = if point m p < 9 && notElem p s
                    then foldl (basin m) (p:s) (neighbours p)
                    else s
basins::[[Int]] -> [(Int,Int)]-> [[(Int,Int)]]
basins m = map (basin m [])

solvePart2 :: [[Int]] -> Int
solvePart2 xs = f $ basins xs $ localMinima (0,0) xs
  where f = product . take 3 . reverse . sort . map (sum . map length)

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input9")
  let lss =  map Text.unpack ls
  let xs1 = [map digitToInt y | y<-lss]
  let rezPart1 = sum $ map (riskLevel xs1) $ localMinima (0,0) xs1
  let rezPart2 = solvePart2 xs1
  let solpart1 = "The answer to Part1: " ++ show rezPart1
  let solpart2 = "The answer to Part2: " ++ show rezPart2
  putStrLn solpart1
  putStrLn solpart2


import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.List.Split
import Data.List
import Debug.Trace

data Point = P Int Int deriving (Show,Eq,Ord)
data Segment = Seg Point Point deriving (Show,Eq,Ord)

isStrait :: Segment->Bool
isStrait (Seg (P x1 y1) (P x2 y2)) = x1==x2 || y1==y2

makeRange::Int->Int->[Int]
makeRange x y = if x<=y then [x..y] else [x,x-1..y]

listPoints:: Segment -> [Point]
listPoints (Seg (P x1 y1) (P x2 y2)) = map (\(x,y)->P x y) (take maxLength infiniteRange)
                                        where infiniteRange = zip (cycle xs) (cycle ys)
                                              maxLength = max (length xs)(length ys)
                                              xs = makeRange x1 x2
                                              ys = makeRange y1 y2
countDuplicatesPoints :: [Point]-> Int
countDuplicatesPoints points = M.size $ M.filter (> 1) pointsMap
  where pointsMap = M.fromListWith(+) $ zip points (repeat 1)

countDuplicatesSegments ::[Segment] -> Int
countDuplicatesSegments segments = countDuplicatesPoints points
  where points=concatMap listPoints segments

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input5")
  let lss =  map Text.unpack ls
  let listOfSegs = map (splitOn "->") lss
  let listOfSegs2 = [map (splitOn ",") seg | seg<-listOfSegs]
  let listOfSegs3 = [[map (\x -> read x::Int) elem | elem<-seg] | seg<-listOfSegs2]
  let listOfPoints2 = [[ P (elem !! 0 ) (elem !! 1) | elem<-seg] | seg<-listOfSegs3]
  let listOfSegments = [Seg (elem !! 0) (elem !! 1) | elem <- listOfPoints2]
  let rezPart1 = countDuplicatesSegments $ filter isStrait listOfSegments
  let rezPart2 = countDuplicatesSegments listOfSegments
  let solpart1 = "The answer to Part1: " ++ show  rezPart1 
  let solpart2 = "The answer to Part2: " ++ show  rezPart2
  putStrLn solpart1
  putStrLn solpart2






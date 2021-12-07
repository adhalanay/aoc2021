import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.MultiSet as MS
import Data.Char (digitToInt)
import Data.List.Split
import Data.List (elemIndex)

listOfDiffs :: [Int]->[Int]
listOfDiffs xs = [sum [abs(x-y)| x<-xs]|y<-xs]

sumOfProg :: Int -> Int
sumOfProg x = x*(x+1) `div` 2

listOfFuels2 :: [Int]->[Int]
listOfFuels2 xs=[sum [sumOfProg (abs(x-y)) | x <-xs] |y<-[0..maximum xs]]

getMin :: [Int]-> Int
getMin xs = case elemIndex (minimum xs) xs of
   Just n -> n
   otherwise -> error "Shouldn't arrive here"

solpart1 :: [Int]->Int
solpart1 xs = minimum (listOfDiffs xs)

solpart2 :: [Int] -> Int
solpart2 xs = minimum $ listOfFuels2 xs 

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input7")
  let lss =  map Text.unpack ls
  let lss0 = splitOn "," $ head lss
  let lssInt = map (\x->read x::Int) lss0
  let testInt = [ 16,1,2,0,4,2,7,1,2,14]
  let lsM = MS.fromList lssInt
  let rezpart1 = solpart1 lssInt
  let rezpart2 = solpart2 lssInt
  let solpart1 = "The answer to Part1: "++ show rezpart1
  let solpart2 = "The answer to Part2: " ++ show rezpart2
  putStrLn solpart1
  putStrLn solpart2


import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.MultiSet as MS
import Data.Char (digitToInt)
import Data.List.Split
import Data.List (elemIndex,sort)

digits :: String -> Maybe Int
digits x= elemIndex x (sort ["abcefg", "cf", "acdeg", "acdfg", "bcdf",
                                   "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"])
  
main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input8")
  let lss =  map Text.unpack ls
  let lss0 = map (splitOn "|")  lss
  let inputPart1 = [x !! 1 | x<- lss0]
  let lss1 = concatMap (splitOn " ") inputPart1
  let rezPart1 = length $ filter (\x-> length x == 7 || length x==2 || length x ==3 ||
                                   length x == 4) lss1
  --let rezpart2 = solpart2 lssInt
  let solpart1 = "The answer to Part1: "++ show rezPart1
  -- let solpart2 = "The answer to Part2: "
  putStrLn solpart1
  --putStrLn solpart2



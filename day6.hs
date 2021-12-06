import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map as Map
import Data.Char (digitToInt)
import Data.List.Split

update :: Int -> [Int]
update x = if x> 0 then [x-1] else [6,8]

generate :: [Int]->[Int]
generate = concatMap update

totalFishFrom :: Int -> [Int]
totalFishFrom c = replicate (c + 1) 1 ++ zipWith (+) totalFishFrom6 totalFishFrom8

totalFishFrom6 :: [Int]
totalFishFrom6 = totalFishFrom 6

totalFishFrom8 :: [Int]
totalFishFrom8 = totalFishFrom 8

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input6")
  let lss =  map Text.unpack ls
  let xs = head lss
  let xs0 = splitOn "," xs
  let lss0 = map (\x->read x::Int) xs0
  let rezpart1 = length (iterate generate lss0 !! 80)
  let countTotalFishAtDay x = sum $ map (\c -> totalFishFrom c !! x) lss0 
  let solpart1 = "The answer to Part1: " ++ show (countTotalFishAtDay 80)
  let solpart2 = "The answer to Part2: " ++ show (countTotalFishAtDay 256)
  putStrLn solpart1
  putStrLn solpart2
 

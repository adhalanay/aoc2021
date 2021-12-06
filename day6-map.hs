import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.MultiSet as MS
import Data.Char (digitToInt)
import Data.List.Split

type School = MS.MultiSet Int


update :: Int -> [Int]
update x = if x> 0 then [x-1] else [6,8]

generate :: School -> School
generate = MS.concatMap update

generation :: Int -> School -> Int
generation n xs = MS.size (iterate generate xs !!n )

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input6")
  let lss =  map Text.unpack ls
  let lss0 = splitOn "," $ head lss
  let lssInt = map (\x->read x::Int) lss0
  let lsM = MS.fromList lssInt
  let rezpart1 = generation 80 lsM
  let rezpart2 = generation 256 lsM
  let solpart1 = "The answer to Part1: "  ++ show rezpart1
  let solpart2 = "The answer to Part2: " ++ show rezpart2
  putStrLn solpart1
  putStrLn solpart2

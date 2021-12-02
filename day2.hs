import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input2")
  let decomp = map Text.words ls
  print decomp
 -- let solpart1 = "The answer to Part1: " ++ (show (countIncs xs))
 -- let solpart2 = "The answer to Part2: " ++ (show (countIncs (sumOfThree xs)))
 -- putStrLn solpart1
 --  putStrLn solpart2
  

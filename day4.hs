import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Char (digitToInt)
import Data.List.Split
import Data.List

--parseInput :: Text -> ([Int],[String])
-- parseInput txt =
-- case Text.lines txt of
--    [] -> 
--addZero::String -> String



main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input4")
  let lss =  map Text.unpack ls
  let draws = splitOn "," $ head lss
  let boards = splitWhen (=="") $ tail lss
  let boardsReadyToUse = [[words w0 | w0<-w] |w<-boards]
  let solpart1 = "The answer to Part1: " 
  let solpart2 = "The answer to Part2: " 
  putStrLn solpart1
  putStrLn solpart2
  print (boardsReadyToUse !! 3)
  print draws
  

  

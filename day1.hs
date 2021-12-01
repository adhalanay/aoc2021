import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


countIncs :: [Int] -> Maybe Int
countIncs xs = countIncsAux 0 xs

countIncsAux :: Int -> [Int] -> Maybe Int
countIncsAux acc _
  | acc < 0 = Nothing
countIncsAux acc xs
  | xs == [] = Just acc
  | xs == [x] = Just acc
  | otherwise = if x < (head xs0)
                then
                  countIncsAux (acc+1) xs0
                else
                  countIncsAux acc xs0
                where x = head xs
                      xs0 = tail xs
sumOfThree :: [Int]->[Int]
sumOfThree xs = [xs !! i + (xs !! (i+1)) + (xs !! (i+2)) | i<-[0..(length xs)-3]]

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input")
  let lss =  map Text.unpack ls
  let xs = [read l:: Int | l<-lss]
  print (countIncs (sumOfThree xs))

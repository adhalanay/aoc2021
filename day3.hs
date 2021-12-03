import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Char (digitToInt)
import Data.List (foldl')

--Part 1
getElem :: Char -> String -> Int->Bool
getElem s word poz = (word !! poz) == s

countZeroes :: [String] -> Int -> Int
countZeroes xs poz = length (filter (\word -> getElem '0' word poz) xs)

countOnes :: [String] -> Int -> Int
countOnes xs poz = length (filter (\word -> getElem '1' word poz) xs)

toDec ::String -> Int --Picked from stackoverflow
toDec = foldl' (\acc x -> acc*2 + digitToInt x) 0


--Part2
mostCommon :: [String]->Int->Char
mostCommon xs poz = if countOnes xs poz < countZeroes xs poz then '0' else '1'

leastCommon :: [String]->Int->Char
leastCommon xs poz = if countOnes xs poz >= countZeroes xs poz then '0' else '1'

selectO2Aux::Int->[String]->[String]
selectO2Aux poz xs = filter (\x-> (x!!poz) == mostCommon xs poz) xs

selectCO2Aux::Int->[String]->[String]
selectCO2Aux poz xs = filter (\x-> (x!!poz) == leastCommon xs poz) xs 

selectO2 :: Int ->[String]->String
selectO2 _ [x] = x
selectO2 poz xs = selectO2 (poz+1) (selectO2Aux poz xs)

selectCO2 :: Int ->[String]->String
selectCO2 _ [x] = x
selectCO2 poz xs = selectCO2 (poz+1) (selectCO2Aux poz xs) 
 
  
main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input3")
  let lss =  map Text.unpack ls
  let l1 = length (head lss)
  let gamma = [if countOnes lss poz <= countZeroes lss poz then '0' else '1' | poz<-[0..l1-1]]
  let epsilon = [if w=='1' then '0' else '1' | w<-gamma]
  let tests = ["00100","11110","10110","10111","10101","01111","00111","11100","10000",
               "11001","00010","01010"]
  let o2 = selectO2 0 lss
  let co2 = selectCO2 0 lss
  let solpart1 = "The answer to Part1: " ++ show (toDec epsilon * toDec gamma)
  let solpart2 = "The answer to Part2: " ++ show (toDec o2 * toDec co2)
  putStrLn solpart1
  putStrLn solpart2

  

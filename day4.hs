import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Char (digitToInt)
import Data.List.Split
import Data.List
import Debug.Trace

--Part 1

drawBoard::String -> [String]->[String]
drawBoard n [] = []
drawBoard n (elem:rest) = if n==elem
                        then "x":drawBoard n rest
                        else elem:drawBoard n rest
isWinnerBoard :: [[String]] -> Bool
isWinnerBoard board = any isComplete board || any isComplete (transpose board)
  where isComplete = all (=="x")

gamePlay :: [String] -> [[[String]]] -> (Maybe Int,[[String]])
gamePlay [] _ = error "No winner"
gamePlay (n:rest) boards = case winBoard of
  [] -> gamePlay rest board0
  (board:_) -> (Just (read n :: Int), board)
  where board0 = [map (drawBoard n) board | board<-boards]
        winBoard = filter isWinnerBoard board0 

findFirstWinner::[String] -> [[[String]]] -> (Maybe Int,[[String]])
findFirstWinner draws boards = (n, board)
  where (n,board)=gamePlay draws boards

findLastWinner::[String]->[[[String]]] -> (Maybe Int,[[String]])
findLastWinner [] _ = (Nothing, [])
findLastWinner [draw] boards = case loserBoards of
  [board] -> (Just (read draw::Int), [])
  board:restBoards -> (Just (read draw::Int), last winnerBoards)
  where boards0 = [map (drawBoard draw) board | board<-boards]
        winnerBoards = filter isWinnerBoard boards0
        loserBoards = filter (not . isWinnerBoard) boards0
findLastWinner (draw:draws) boards = case loserBoards of
  [board] -> findFirstWinner draws loserBoards
  board:restBoards -> findLastWinner draws loserBoards
  _ -> error "Shouldn't arrive here"
  where boards0 = [map (drawBoard draw) board | board<-boards]
        loserBoards = filter (not . isWinnerBoard) boards0

computeAnswer::(Int,[[String]]) -> Int
computeAnswer (n,board) = n * sum boardInts
  where boardInts = [(\ x -> read x :: Int) elem | elem <- concat board, elem /= "x"]

computeAnswerMaybe::(Maybe Int, [[String]])->Maybe Int
computeAnswerMaybe (Nothing, board) = Nothing
computeAnswerMaybe (Just n, board) = Just (computeAnswer (n,board))

main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input4")
  let lss =  map Text.unpack ls
  let draws = splitOn "," $ head lss
  let testDraws =["7","4","9","5","11","17","23","2","0","14","21","24","10","16","13","6",
                  "15","25","12","22","18","20","8","19","3","26","1"]
  let testBoards = [[["22","13","17","11","0"],["8","2","23","4","24"],["21","9","14","16",
                                                                       "7"],
                     ["6","10","3","18","5"],["1","12","20","15","19"]],
                     [["3","15","0","2","22"],["9","18","13","17","5"],["19","8","7","25",
                                                                        "23"],
                       ["20", "11","10", "24","4"],["14", "21", "16", "12","6"]],
                     [["14", "21", "17", "24","4"],["10", "16", "15","9","19"],
                      ["18", "8","23","26","20"],["22","11","13","6","5"],
                      ["2", "0","12","3",  "7"]]]
  let boards = splitWhen (=="") $ tail lss
  let boardsReadyToUse = [[words w0 | w0<-w] |w<-boards]
  let firstWinner = findFirstWinner draws boardsReadyToUse
  let lastWinner = findLastWinner draws boardsReadyToUse
  let firstWinnerTest = findFirstWinner testDraws testBoards
  let lastWinnerTest = findLastWinner testDraws testBoards
  let rezpart1 = computeAnswerMaybe firstWinner
  let rezpart2 = computeAnswerMaybe lastWinner
  let solpart1 = "The answer to Part1: " ++ show (computeAnswerMaybe firstWinner)
  let solpart2 = "The answer to Part2: " ++ show (computeAnswerMaybe lastWinner)
  putStrLn solpart1
  putStrLn solpart2






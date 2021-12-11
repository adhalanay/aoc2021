import Data.List (elemIndex, sort)
import Data.Map (fromList, (!))
import qualified Data.Functor

close :: Char -> Char
close = (!) $ fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

score :: Char -> Int
score = (!) $ fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

score':: Char -> Int
score' c = let Just s = elemIndex c "_)]}>" in s

parse ::String->String->Either Char String
parse (a:as) (x:xs)
  | x== close a = parse as xs
  | x `elem` "(<{[" = parse (x:a:as) xs
  | otherwise = Left x
parse as [] = Right $ map close as
parse [] (x:xs)
  | x `elem` "(<{[" = parse [x] xs
  | otherwise = Left x

main :: IO ( )
main = do
  ls <- readFile "input10" Data.Functor.<&> lines
  let (openings,closings) =
        foldl (\(x,y) a -> case a of
                  Left b -> (b:x,y)
                  Right b -> (x, b:y)) ([],[])
        $ map (parse "") ls
  let rezPart1 = sum $ map score openings
  let rezPart2 = sort (map (foldl (\n a -> n * 5 + score' a) 0) closings) !! div (length closings) 2
  let solpart1 = "The answer to Part1: " ++ show rezPart1
  let solpart2 = "The answer to Part2: " ++ show rezPart2
  putStrLn solpart1
  putStrLn solpart2



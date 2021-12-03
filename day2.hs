import System.IO
import qualified Data.Text as Text
import qualified Data.Text.IO as Text


data Poz = P Int Int deriving Show
data Instr = Inst String String deriving Show

oneMoveP1 :: Poz -> Instr -> Poz
oneMoveP1 (P horiz vert)(Inst direct amount)
  | direct == "up" = P horiz (vert-amountToInt)
  | direct == "down" = P horiz (vert+amountToInt)
  | direct == "forward" = P (horiz+amountToInt) vert
  | otherwise = P horiz vert
  where amountToInt = (read amount)::Int

-- Part 1
move :: (Poz -> Instr->Poz)->Poz -> [Instr] -> Poz
move f object xs = case xs of [] -> object
                              [x] -> f object x
                              x:xs0 -> move f (f object x) xs0

--Part 2
data Poz2 = P2 Int Int Int deriving Show
oneMoveP2 :: Poz2 -> Instr -> Poz2
oneMoveP2 (P2 horiz vert aim)(Inst direct amount)
  | direct == "down" = P2 horiz vert (aim+amountToInt)
  | direct == "up" = P2 horiz vert (aim-amountToInt)
  | direct == "forward" = P2 (horiz+amountToInt) (vert+amountToInt*aim) aim
  | otherwise = P2 horiz vert aim
  where amountToInt = (read amount)::Int
move2 :: (Poz2 -> Instr->Poz2)->Poz2 -> [Instr] -> Poz2
move2 f object xs = case xs of [] -> object
                               [x] -> f object x
                               x:xs0 -> move2 f (f object x) xs0
 
main :: IO ( )
main = do
  ls <- fmap Text.lines (Text.readFile "input2")
  let lss =  map Text.unpack ls
  let decomp = map words lss
  let instructions =[Inst (w !! 0) (w !! 1) | w <- decomp]
  let init1 = P 0 0
  let init2 = P2 0 0 0
  let final1 = move oneMoveP1 init1 instructions
  let final2 = move2 oneMoveP2 init2 instructions
  let P h1 v1 = final1
  let P2 h2 v2 aim = final2
  let solpart1 = "The answer to Part1: " ++ show(h1*v1)
  let solpart2 = "The answer to Part2: " ++ show(h2*v2)
  putStrLn solpart1
  putStrLn solpart2
 
  

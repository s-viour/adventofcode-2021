module Day4
  ( dayMain1
  , dayMain2
  ) where

import Data.List ( maximumBy, minimumBy )
import Data.Matrix ( Matrix )
import Data.List.Split ( splitOn )
import qualified Data.Matrix as Matrix
type Board = Matrix (Int, Bool)


makeBoard :: String -> Board
makeBoard input = mapBoard (Matrix.fromList 5 5 (map read (words input)))
  where
    mapBoard = Matrix.mapPos (\(r,c) a -> (a, False))

getDrawList :: String -> [Int]
getDrawList input = map read (splitOn "," input)

getBoards :: [String] -> [Board]
getBoards = map makeBoard

getData :: String -> ([Int], [Board])
getData s = (getDrawList (head rawData), getBoards (tail rawData))
  where rawData = splitOn "\n\n" s

playBoard :: Int -> Board -> Board
playBoard i = Matrix.mapPos mapFxn
  where
    mapFxn (_,_) (v,t) =
      if v == i then (v, True)
      else (v, t)

checkBoard :: Board -> Bool
checkBoard b = rows || cols
  where
    allTrue vals = all snd vals
    rows = any (\i -> allTrue (Matrix.getRow i b)) [1..5]
    cols = any (\i -> allTrue (Matrix.getCol i b)) [1..5]

finishBoard :: [Int] -> Int -> Board -> (Int, Board)
finishBoard [] c b = (c, b)
finishBoard (i:rest) c b
  | checkBoard b = (c, b)
  | otherwise = finishBoard rest (c + 1) (playBoard i b)

scoreBoard :: Board -> Int
scoreBoard = foldl scoreb 0
  where
    scoreb v (a, True) = v
    scoreb v (a, False) = v + a

solve1 :: [Int] -> [(Int, Board)] -> Int
solve1 drawList bs =
  let
    winningBoard = minimumBy (\(a, _) (b, _) -> if a < b then LT else GT) bs
    score = (scoreBoard . snd) winningBoard
    draw = (drawList !! (fst winningBoard - 1))
  in
    score * draw
  
solve2 :: [Int] -> [(Int, Board)] -> Int
solve2 drawList bs =
  let
    winningBoard = maximumBy (\(a, _) (b, _) -> if a < b then LT else GT) bs
    score = (scoreBoard . snd) winningBoard
    draw = (drawList !! (fst winningBoard - 1))
  in
    score * draw

dayMain1 :: IO ()
dayMain1 = do
  contents <- getContents
  let (drawList, boards) = getData contents
  let bs = map (finishBoard drawList 0) boards
  print $ solve1 drawList bs

dayMain2 :: IO ()
dayMain2 = do
  contents <- getContents
  let (drawList, boards) = getData contents
  let bs = map (finishBoard drawList 0) boards
  print $ solve2 drawList bs

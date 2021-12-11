module Day5
  ( dayMain1
  , dayMain2
  ) where

import Data.Char ( isDigit )
import Data.Map (Map)
import qualified Data.Map as Map

type Point = (Int, Int)
newtype Line = Line (Point, Point)


instance Show Line where
  show (Line (p1, p2)) = show p1 ++ " -> " ++ show p2


parsePoint :: String -> (Point, String)
parsePoint s =
  let
    r0 = dropWhile (not . isDigit) s
    (n1, r1) = span isDigit r0
    (n2, rest) = span isDigit (dropWhile (not . isDigit) r1)
  in
    ((read n1, read n2), rest)

parseLine :: String -> (Line, String)
parseLine s =
  let
    (p1, r1) = parsePoint s
    (p2, rest) = parsePoint (dropWhile (not . isDigit) r1)
  in
    (Line (p1, p2), rest)

parseLines :: String -> [Line] -> [Line]
parseLines "" lns = lns
parseLines s lns =
  let
    (line, rest) = parseLine s
  in
    parseLines rest (lns ++ [line])

parseInput :: String -> [Line]
parseInput contents = parseLines contents []

straightLines :: [Line] -> [Line]
straightLines = filter isStraight
  where
    isStraight :: Line -> Bool
    isStraight (Line ((x1, y1), (x2, y2))) = x1 == x2 || y1 == y2

diagLines :: [Line] -> [Line]
diagLines = filter isDiag
  where
    isDiag :: Line -> Bool
    isDiag (Line ((x1, y1), (x2, y2))) =
      x1 == y2 && x2 == y1 || x1 == x2 && y1 == y2

allPoints :: Line -> [Point]
allPoints (Line ((x1, y1), (x2, y2))) =
  let
    f1
      | x1 > x2 = (\x -> x - 1)
      | x1 < x2 = (+ 1)
      | otherwise = (+ 0)
    f2
      | y1 > y2 = (\y -> y - 1)
      | y1 < y2 = (+ 1)
      | otherwise = (+ 0)
    pts = iterate (\(a, b) -> (f1 a, f2 b)) (x1, y1)
  in
    takeWhile (\(a, b) -> a /= x2 || b /= y2) pts ++ [(x2, y2)]

sumPoints :: [Point] -> Map Point Int -> Map Point Int
sumPoints [] m = m
sumPoints (p:rest) m
  | Map.member p m = sumPoints rest (Map.adjust (+ 1) p m)
  | otherwise = sumPoints rest (Map.insert p 1 m)

getOverlaps :: Map Point Int -> [Point]
getOverlaps m = (map fst . filter (\(k, v) -> v >= 2)) (Map.toList m)

dayMain1 :: IO ()
dayMain1 = do
  contents <- getContents
  let input = parseInput contents
  let straightPts = (map allPoints . straightLines) input
  let pts = concat straightPts
  let sums = sumPoints pts Map.empty
  print $ (length . getOverlaps) sums

dayMain2 :: IO ()
dayMain2 = do
  contents <- getContents
  let input = parseInput contents
  let pts = concatMap allPoints input
  let sums = sumPoints pts Map.empty
  print $ (length . getOverlaps) sums

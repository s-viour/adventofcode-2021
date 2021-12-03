module Day2
  ( dayMain1
  , dayMain2
  ) where

import System.IO ( hPutStrLn, stderr )
import System.Environment ( getArgs )

data Direction = Down Int | Up Int | Forward Int


parseDirection :: String -> Direction
parseDirection v
  | dir == "down" = Down amnt
  | dir == "up" = Up amnt
  | dir == "forward" = Forward amnt
  | otherwise = error "this shouldn't happen!"
  where
    (dir, stramnt) = span (/= ' ') v
    amnt = read stramnt :: Int

getDirections :: IO [Direction]
getDirections = do
  contents <- getContents
  let lns = lines contents
  return (map parseDirection lns)

handleDirection1 :: Direction -> (Int, Int) -> (Int, Int)
handleDirection1 d (x, y) = case d of
  Down amnt -> (x, y + amnt)
  Up amnt -> (x, y - amnt)
  Forward amnt -> (x + amnt, y)

handleDirection2 :: Direction -> (Int, Int, Int) -> (Int, Int, Int)
handleDirection2 d (a, x, y) = case d of
  Down aim -> (a + aim, x, y)
  Up aim -> (a - aim, x, y)
  Forward amnt -> (a, x + amnt, y + a * amnt)

solve1 :: [Direction] -> Int
solve1 dirs = uncurry (*) $ foldr handleDirection1 (0, 0) dirs

solve2 :: [Direction] -> Int
solve2 dirs = (\(_, a, b) -> a * b) $ foldr handleDirection2 (0, 0, 0) dirs

dayMain1 :: IO ()
dayMain1 = getDirections >>= (print . solve1)

dayMain2 :: IO ()
dayMain2 = getDirections >>= (print . solve2 . reverse)

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

main :: IO ()
main = do
  args <- getArgs
  if null args then usage
  else if head args == "solve1" then 
    getDirections >>= (print . solve1)
  else if head args == "solve2" then
    getDirections >>= (print . solve2 . reverse)
  else
    usage

usage :: IO ()
usage = hPutStrLn stderr "usage: day2 [solve1|solve2]"
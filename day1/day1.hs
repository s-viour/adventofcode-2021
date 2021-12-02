module Main where

import System.Environment ( getArgs ) 
import System.IO ( stderr, hPutStrLn )


getValues :: IO [Int]
getValues = map (read :: String -> Int) . lines <$> getContents

createWindows :: [Int] -> [(Int, Int, Int)]
createWindows values = zip3 values (drop 1 values) (drop 2 values)

createSumList :: [(Int, Int, Int)] -> [Int]
createSumList = map tupleSum
  where
    tupleSum :: (Int, Int, Int) -> Int
    tupleSum (a, b, c) = a + b + c

createPairs :: [Int] -> [(Int, Int)]
createPairs values = zip values (tail values)

countIncreasing :: [(Int, Int)] -> Int
countIncreasing = length . filter (uncurry (<))

solve1 :: [Int] -> Int
solve1 = countIncreasing . createPairs

solve2 :: [Int] -> Int
solve2 = countIncreasing . createPairs . createSumList . createWindows

getSolver :: [String] -> Maybe ([Int] -> Int)
getSolver [] = Nothing
getSolver args
  | head args == "solve1" = Just solve1
  | head args == "solve2" = Just solve2
  | otherwise = Nothing

main :: IO ()
main = do
  values <- getValues
  args <- getArgs
  case getSolver args of
    Nothing -> usage
    Just s -> (print .s) values
  where
    usage = hPutStrLn stderr "usage: day1 [solve1|solve2]"
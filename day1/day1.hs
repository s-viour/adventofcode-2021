module Main where


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

solve :: [Int] -> Int
solve = countIncreasing . createPairs . createSumList . createWindows

main :: IO ()
main = getValues >>= (print . solve)
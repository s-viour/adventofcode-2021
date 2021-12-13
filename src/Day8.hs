module Day8
  ( dayMain1
  , dayMain2
  ) where


type SignalValue = String

parseInputLine :: String -> [SignalValue]
parseInputLine = words . drop 1 . dropWhile (/= '|')

parseInput :: String -> [SignalValue]
parseInput = concatMap parseInputLine . lines

countSignals1478 :: [SignalValue] -> Int
countSignals1478 = length . filter is1478
  where
    is1478 :: SignalValue -> Bool
    is1478 s
      | length s == 2 = True
      | length s == 4 = True
      | length s == 3 = True
      | length s == 7 = True
      | otherwise = False

dayMain1 :: IO ()
dayMain1 = getContents >>= (print . countSignals1478 . parseInput)

dayMain2 :: IO ()
dayMain2 = putStrLn "unimplemented!"
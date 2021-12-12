module Day7
  ( dayMain1
  , dayMain2
  ) where

import Data.List.Split ( splitOn )

type CrabSubmarines = [Int]
type CostFxn = Int -> Int -> Int


parseInput :: String -> CrabSubmarines
parseInput = map read . splitOn ","

costOfAligning1 :: CostFxn
costOfAligning1 a b = abs (a - b)

costOfAligning2 :: CostFxn
costOfAligning2 a b = sum [1..(costOfAligning1 a b)]

fuelCostOfAligning :: CostFxn -> Int -> CrabSubmarines -> Int
fuelCostOfAligning cost i = sum . map (cost i)

minimumFuelCost :: CostFxn -> CrabSubmarines -> Int
minimumFuelCost cost crabs = (minimum . map (\a -> a crabs)) fxns
  where
    fxns = map (fuelCostOfAligning cost) [(minimum crabs)..(maximum crabs)]

dayMain1 :: IO ()
dayMain1 = getContents >>= (print . cost . parseInput)
  where
    cost = minimumFuelCost costOfAligning1

dayMain2 :: IO ()
dayMain2 = getContents >>= (print . cost . parseInput)
  where
    cost = minimumFuelCost costOfAligning2

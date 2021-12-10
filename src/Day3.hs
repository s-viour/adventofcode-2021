module Day3
  ( dayMain1
  , dayMain2
  ) where

import System.IO ( hPutStrLn, stderr )
import Data.Bits ( testBit, shiftR, complement )
import Data.Char ( digitToInt )

toDec :: String -> Int
toDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

toDecArr :: [Int] -> Int
toDecArr = foldl (\acc x -> acc * 2 + x) 0

getNumbers :: String -> [Int]
getNumbers lns = map toDec (lines lns)

highestSetBit :: Int -> Int
highestSetBit b = (length . takeWhile (/= 0)) (iterate (`shiftR` 1) b) - 1

mostCommonBit :: Int -> [Int] -> Int
mostCommonBit pos nums =
  if ones >= zeros then 1
  else 0
  where
    extracted = map (`testBit` pos) nums
    ones = (length . filter (== True)) extracted
    zeros = length extracted - ones

leastCommonBit :: Int -> [Int] -> Int
leastCommonBit pos nums = if mostCommonBit pos nums == 1 then 0 else 1

iterateBits :: (Int -> [Int] -> Int) -> [Int] -> Int -> [Int] -> [Int]
iterateBits f bits 0 numbers = bits ++ [f 0 numbers]
iterateBits f bits n numbers = bits ++ [f n numbers] ++ iterateBits f bits (n - 1) numbers

co2OxRate :: (Int -> [Int] -> Int) -> [Int] -> Int -> [Int]
co2OxRate _ [x] _ = [x]
co2OxRate bitFxn nums pos = co2OxRate bitFxn (filter fxn nums) (pos - 1)
  where
    mc = bitFxn pos nums
    fxn = 
      if mc == 1 then (`testBit` pos)
      else (\n -> not $ n `testBit` pos)

oxygenRate :: [Int] -> Int -> [Int]
oxygenRate = co2OxRate mostCommonBit

co2Rate :: [Int] -> Int -> [Int]
co2Rate = co2OxRate leastCommonBit

gammaRate :: [Int] -> Int -> [Int] -> [Int]
gammaRate = iterateBits mostCommonBit

epsilonRate :: [Int] -> Int -> [Int] -> [Int]
epsilonRate = iterateBits leastCommonBit

dayMain1 :: IO ()
dayMain1 = do
  contents <- getContents
  let numbers = getNumbers contents
  let bitPos = maximum (map highestSetBit numbers)
  let gr = (toDecArr . gammaRate [] bitPos) numbers
  let er = (toDecArr . epsilonRate [] bitPos) numbers
  print (gr * er)

dayMain2 :: IO ()
dayMain2 = do
  contents <- getContents
  let numbers = getNumbers contents
  let bitPos = maximum (map highestSetBit numbers)
  let [o2] = oxygenRate numbers bitPos
  let [co2] = co2Rate numbers bitPos
  print (o2 * co2)

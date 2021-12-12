module Day6
  ( dayMain1
  , dayMain2
  ) where

import Data.List.Split ( splitOn )
import Data.Vector ( Vector, (//), (!) )
import qualified Data.Vector as Vector

type Fish = Int
type FishPopulation = Vector Fish


emptyPopulation :: FishPopulation
emptyPopulation = Vector.fromList [0, 0, 0, 0, 0, 0, 0, 0, 0]

updatePopulation :: Int -> FishPopulation -> FishPopulation
updatePopulation i pop
  | Vector.length pop == 9 = pop // [(i, (pop ! i) + 1)]
  | otherwise = error "invalid fish population!"

parseInput :: String -> FishPopulation
parseInput input = foldr updatePopulation emptyPopulation parsed
  where
    parsed :: [Fish]
    parsed = (map read . splitOn ",") input

{-
  the highest lifespan a fish can have is 8
  if we take each position in the list to be the amount of days a fish
  has, then the next population is simply the list shifted left.
  
  additionally, the 0th position in the list moves to the 6th position,
  and it adds new fish to the 8th position
  
  0 1 2 3 4 5 6   7  8
  --------------------
  a b c d e f g   h  i
  b c d e f g a+h i  a

-}
nextPopulation :: FishPopulation -> FishPopulation
nextPopulation v
  | Vector.length v == 9 = Vector.update v updateList
  | otherwise = error "invalid fish population!"
  where
    updateList :: Vector (Int, Fish)
    updateList = Vector.fromList
      [ (0, v ! 1), (1, v ! 2), (2, v ! 3), (3, v ! 4)
      , (4, v ! 5), (5, v ! 6), (6, (v ! 0) + (v ! 7))
      , (7, v ! 8), (8, v ! 0)
      ]

progressDays :: Int -> FishPopulation -> FishPopulation
progressDays n = (!! n) . iterate nextPopulation

totalFishAfterDays :: Int -> FishPopulation -> Int
totalFishAfterDays n = sum . progressDays n

dayMain1 :: IO ()
dayMain1 = getContents >>= (print . totalFishAfterDays 80 . parseInput)

dayMain2 :: IO ()
dayMain2 = getContents >>= (print . totalFishAfterDays 256 . parseInput)

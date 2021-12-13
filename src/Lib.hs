module Lib
  ( days
  ) where

import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8

days :: Map Int (IO (), IO ())
days = Map.fromList
  [ (1, (Day1.dayMain1, Day1.dayMain2))
  , (2, (Day2.dayMain1, Day2.dayMain2))
  , (3, (Day3.dayMain1, Day3.dayMain2))
  , (4, (Day4.dayMain1, Day4.dayMain2))
  , (5, (Day5.dayMain1, Day5.dayMain2))
  , (6, (Day6.dayMain1, Day6.dayMain2))
  , (7, (Day7.dayMain1, Day7.dayMain2))
  , (8, (Day8.dayMain1, Day8.dayMain2))
  ]

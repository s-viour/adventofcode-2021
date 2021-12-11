module Lib
  ( days
  ) where

import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4

days :: Map Int (IO (), IO ())
days = Map.fromList
  [ (1, (Day1.dayMain1, Day1.dayMain2))
  , (2, (Day2.dayMain1, Day2.dayMain2))
  , (3, (Day3.dayMain1, Day3.dayMain2))
  , (4, (Day4.dayMain1, Day4.dayMain2))
  ]

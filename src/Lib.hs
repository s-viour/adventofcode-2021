module Lib
  ( days
  ) where

import Data.Map ( Map )
import qualified Data.Map as Map
import qualified Day1
import qualified Day2

days :: Map Int (IO (), IO ())
days = Map.fromList
  [ (1, (Day1.dayMain1, Day1.dayMain2))
  , (2, (Day2.dayMain1, Day2.dayMain2))
  ]

module Main where


import System.Environment ( getArgs )
import System.IO ( hPutStrLn, stderr )
import qualified Data.Map as Map
import Lib ( days )


data AppConfig = AppConfig
  { day  :: Int
  , part :: Int
  }

parseArgs :: [String] -> Maybe AppConfig
parseArgs [] = Nothing
parseArgs [_] = Nothing
parseArgs (sday:spart:_) = Just AppConfig
  { day = read sday :: Int
  , part = read spart :: Int
  }

runPart :: Int -> (IO (), IO ()) -> IO ()
runPart p (p1, p2)
  | p == 1 = p1
  | p == 2 = p2
  | otherwise = hPutStrLn stderr "invalid part!"

runDay :: Maybe AppConfig -> IO ()
runDay cfg =
  case cfg of
    Just (AppConfig day part) ->
      case Map.lookup day days of
        Just parts -> runPart part parts
        Nothing -> hPutStrLn stderr "day not implemented yet!"
    Nothing -> usage

main :: IO ()
main = getArgs >>= (runDay . parseArgs)

usage :: IO ()
usage = hPutStrLn stderr "usage: aoc2021 [day number] [part number]"
module Main where

import Prelude as P
import Data.Map.Strict as M
import System.Environment
import Numeric

type InputStats = [(String,String)]
type OutputStats = Map String Double

type Value = Double

data CompareResult = Result
    { name :: String
    , baseline :: Value
    , changed :: Value
    , absDiff :: Value
    , relDiff :: Value
    } deriving Show

printResult :: CompareResult -> String
printResult result =
    size_name (name result) ++ ":" ++ (showFFloat (Just 4) (relDiff result) "" ) ++ "(" ++ (show $ changed result) ++ "/" ++ (show $ baseline result) ++ ")"

readStats :: InputStats -> OutputStats
readStats [] = mempty
readStats ((what,number):todo) =
    insert what (read number) $ readStats todo

compareMetric :: String -> Double -> Double -> CompareResult
compareMetric what v1 v2 =
    Result what v1 v2 (v2 - v1) (v2/v1)

results :: OutputStats -> OutputStats -> [CompareResult]
results base patch =
    M.elems $ intersectionWithKey compareMetric base patch

size_name :: String -> String
size_name s
    | len < 28
    = replicate (28-len) ' ' ++ s
    | otherwise
    = P.take 28 s
  where
    len = length s

main = do
    (base:patch:_) <- getArgs
    baseResults <- read <$> readFile base :: IO InputStats
    patchResults <- read <$> readFile patch :: IO InputStats
    let compResults = P.filter (\r -> absDiff r /= 0) $! results (readStats baseResults) (readStats patchResults)

    mapM_ (putStrLn . printResult) $ compResults

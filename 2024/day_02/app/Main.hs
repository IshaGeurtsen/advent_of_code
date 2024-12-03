module Main where
import Data.List (lines, words)
import GHC.Real (reduce)

newtype Report = Report [Integer] deriving Show

toInt :: String -> Integer
toInt s = read s :: Integer

boolToInt :: Bool -> Integer
boolToInt b
 | b = 1
 | otherwise = 0

generateReport :: String -> Report
generateReport reportLine = do
    Report(map toInt (words reportLine))

generateReports :: String -> [Report]
generateReports s = do
    map generateReport (lines s)


validate :: [Integer] -> (Integer -> Integer -> Bool) -> Bool
validate l f
 | length l < 2 = True
 | otherwise = f (head l) ((head.tail) l) && validate (tail l) f

tolorantValidate :: [Integer] -> (Integer -> Integer -> Bool) -> Bool
tolorantValidate l f
 | validate l f = True
    


isDecreasing :: Integer -> Integer -> Bool
isDecreasing a b = do 
    let x = a - b
    0 < x && x < 4

isIncreasing :: Integer -> Integer -> Bool
isIncreasing a b = do
    let x = b - a
    0 < x && x < 4

isSafe :: ([Integer] -> (Integer -> Integer -> Bool) -> Bool) -> Report -> Bool
isSafe validator (Report values) = validator values isDecreasing || validator values isIncreasing

count :: [Bool] -> Integer
count = sum.map boolToInt

program :: String -> String
program text = do 
    let reports = generateReports text
    let part1 = "part 1: " ++ show (count (map (isSafe validate) reports))
    let part2 = "part 2: " ++ show (count (map (isSafe validate) reports))
    part1 ++ "\n" ++ part2

main :: IO ()
main = do
    text <- readFile ".input"
    (putStr.program) text

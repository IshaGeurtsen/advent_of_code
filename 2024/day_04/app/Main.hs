module Main where

import Data.List
import Data.Maybe

data Point = Point Int Int deriving (Show)

originPoint = Point 0 0

pointX (Point x y) = x

pointY (Point x y) = y

lookupPointIndex :: Point -> Point -> Maybe Int
lookupPointIndex (Point bx by) (Point x y)
  | x < 0 = Nothing
  | bx < x = Nothing
  | y < 0 = Nothing
  | by < y = Nothing
  | otherwise = Just (y * (bx + 1) + x)

lookupPoint :: String -> Point -> Point -> Maybe Char
lookupPoint s b (Point x y) =
  case lookupPointIndex b (Point x y) of
    Nothing -> Nothing
    Just i -> Just (s !! i)

xmas :: String -> Int
xmas (x : m : a : s : sx)
  | x /= 'X' = 0
  | m /= 'M' = 0
  | a /= 'A' = 0
  | s /= 'S' = 0
  | otherwise = 1

_XmasCount :: Int -> String -> Int
_XmasCount acc s
  | length s >= 4 = _XmasCount (acc + xmas s) (tail s)
  | otherwise = acc

xmasCount :: String -> Int
xmasCount = _XmasCount 0

_Bounds :: Point -> String -> Point
_Bounds (Point x y) s
  | null s = Point x y
  | y == 0 && head s /= '\n' = _Bounds (Point (x + 1) y) (tail s)
  | head s == '\n' = _Bounds (Point x (y + 1)) (tail s)
  | otherwise = _Bounds (Point x y) (tail s)

bounds :: String -> Point
bounds = _Bounds originPoint

program :: String -> String
program s = do
  let part_1_value = xmasCount s :: Int
  let b = bounds s
  "part 1: "
    ++ show part_1_value
    ++ "\n"
    ++ "bounds: "
    ++ show b
    ++ "\n"

allPoints :: String -> [(Char, Point)]
allPoints = _AllPoints [] originPoint

_AllPoints :: [(Char, Point)] -> Point -> String -> [(Char, Point)]
_AllPoints points cursor "" = points
_AllPoints points cursor (c : sx)
  | c == 'X' || c == 'M' || c == 'A' || c == 'S' = _AllPoints (points ++ [(c, cursor)]) (Point (1 + pointX cursor) (pointY cursor)) sx
  | c == '\n' = _AllPoints points (Point 0 (1 + pointY cursor)) sx
  | otherwise = [(c, cursor)]

lookupMapsCorrectlyTest :: String -> (Char, Point) -> [(Char, Point, Char, Maybe Int)]
lookupMapsCorrectlyTest s (c, p) = do
  let b = bounds s
  let maybeC = lookupPoint s b p
  case maybeC of
    Nothing -> [(c, p, '?', lookupPointIndex b p)]
    Just testC ->
      ([(c, p, testC, lookupPointIndex b p) | testC /= c])

lookupMapsCorrectly :: String -> String
lookupMapsCorrectly text = do
  let points = allPoints text
  let incorrectPoints = foldr1 (++) (map (lookupMapsCorrectlyTest text) points)
  if null incorrectPoints
    then "."
    else "\nlookupMapsCorrectly > " ++ show incorrectPoints

tests :: String -> String
tests text = foldr1 (++) ["tests: ", lookupMapsCorrectly text]

main :: IO ()
main = do
  text <- readFile ".input"
  putStr (program text)
  putStr (tests text)

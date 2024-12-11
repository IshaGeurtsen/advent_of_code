module Main where

import Data.List
import Data.Maybe

data Point = Point Int Int
  deriving (Show)

newtype Bound = Bound Point
  deriving (Show)

newtype Direction = Direction Point

originPoint = Point 0 0

searchDirections =
  map
    (newDirection . pointFromTuple)
    [ (1, 1),
      (0, 1),
      (-1, 1),
      (1, 0),
      (0, 0),
      (-1, 0),
      (1, -1),
      (0, -1),
      (-1, -1)
    ]

pointFromTuple (x, y) = Point x y

newDirection = Direction

pointX (Point x y) = x

pointY (Point x y) = y

step (Point aX aY) (Direction (Point bX bY)) stride = Point (aX + bX * stride) (aY + bY * stride)

walk :: Point -> Direction -> Int -> [Point]
walk point direction distance
  | distance == 0 = [point]
  | distance > 0 = walk point direction (distance - 1) ++ [step point direction distance]

lookupPointIndex :: Bound -> Point -> Maybe Int
lookupPointIndex (Bound (Point bx by)) (Point x y)
  | x < 0 = Nothing
  | bx < x = Nothing
  | y < 0 = Nothing
  | by <= y = Nothing
  | otherwise = Just (y * (bx + 1) + x)

lookupPoint :: String -> Bound -> Point -> Maybe Char
lookupPoint s b (Point x y) =
  case lookupPointIndex b (Point x y) of
    Nothing -> Nothing
    Just i -> Just (s !! i)

wordWalk :: String -> String -> Bound -> Point -> Direction -> Bool
wordWalk w s b p d = do
  let points = walk p d (-1 + length w)
  let chars = map (maybeToList . lookupPoint s b) points
  let xmasWord = foldr1 (++) chars :: String
  xmasWord == w

xmasWalk = wordWalk "XMAS"

xmas :: String -> Bound -> (Char, Point) -> Int
xmas s b (c, p) =
  if c == 'X'
    then sum (map (boolAsInt . xmasWalk s b p) searchDirections)
    else 0

boolAsInt :: Bool -> Int
boolAsInt b =
  if True
    then 1
    else 0

_XmasCount :: Int -> String -> Bound -> [(Char, Point)] -> Int
_XmasCount acc s b points
  | null points = acc
  | otherwise = _XmasCount (acc + xmas s b (head points)) s b (tail points)

xmasCount :: String -> Bound -> [(Char, Point)] -> Int
xmasCount = _XmasCount 0

masCount :: String -> Bound -> [(Char, Point)] -> Int
masCount = _MasCount 0

_MasCount :: Int -> String -> Bound -> [(Char, Point)] -> Int
_MasCount acc s b [] = acc
_MasCount acc s b (('A', p) : px) =
  if isMas s b p
    then 1 + _MasCount acc s b px
    else _MasCount acc s b px
_MasCount acc s b ((c, p) : px) = _MasCount acc s b px

isMas :: String -> Bound -> Point -> Bool
isMas s b p = do
  let left = (step p ((newDirection . pointFromTuple) (-1, -1)) 1, (newDirection . pointFromTuple) (1, 1))
  let right = (step p ((newDirection . pointFromTuple) (-1, 1)) 1, (newDirection . pointFromTuple) (1, -1))
  let leftChecked = uncurry (wordWalk "SAM" s b) left || uncurry (wordWalk "MAS" s b) left
  let rightChecked = uncurry (wordWalk "SAM" s b) right || uncurry (wordWalk "MAS" s b) right
  leftChecked && rightChecked

_Bounds :: Point -> String -> Bound
_Bounds (Point x y) s
  | null s = Bound (Point x y)
  | y == 0 && head s /= '\n' = _Bounds (Point (x + 1) y) (tail s)
  | head s == '\n' = _Bounds (Point x (y + 1)) (tail s)
  | otherwise = _Bounds (Point x y) (tail s)

bounds :: String -> Bound
bounds = _Bounds originPoint

program :: String -> String
program s = do
  let bound = bounds s
  let points = allPoints s
  let part_1_value = xmasCount s bound points :: Int
  let part_2_value = masCount s bound points :: Int
  "part 1: "
    ++ show part_1_value
    ++ "\n"
    ++ "bounds: "
    ++ show bound
    ++ "\n"
    ++ "part 2: "
    ++ show part_2_value
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
tests text =
  foldr1
    (++)
    [ "tests: ",
      lookupMapsCorrectly text
    ]

main :: IO ()
main = do
  text <- readFile ".input"
  putStr (program text)
  putStr (tests text)

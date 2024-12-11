module Main where

xmas :: String -> Int
xmas (x:m:a:s:sx)
 | x /= 'X' = 0
 | m /= 'M' = 0
 | a /= 'A' = 0
 | s /= 'S' = 0
 | otherwise = 1

_XmasCount :: Int -> String -> Int
_XmasCount acc s
 | length s >= 4 = _XmasCount (acc+xmas s) (tail s)
 | otherwise = acc

xmasCount ::  String -> Int
xmasCount = _XmasCount 0

_Bounds :: (Int, Int) -> String -> (Int, Int)
_Bounds (x, y) s
 | null s = (x, y)
 | y == 0 && head s /= '\n' = _Bounds (x+1, y) (tail s)
 | head s == '\n' = _Bounds (x, y+1) (tail s)
 | otherwise = _Bounds (x, y) (tail s)

bounds :: String -> (Int, Int)
bounds = _Bounds (0, 0)

program :: String -> String
program s = do
    let part_1_value = xmasCount s :: Int
    let b = bounds s
    "part 1: " ++ show part_1_value ++ "\n"
        ++ "bounds: " ++ show b ++ "\n"

main :: IO ()
main = do
    text <- readFile ".input"
    putStr (program text)

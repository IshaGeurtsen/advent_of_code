module Main where
import qualified Data.Text as T
import Data.Char (isDigit, digitToInt)
import Numeric (readInt)


-- unused because my code did 123 -> 10*1+(10*2 + (3))
charToInt :: Char -> Integer
charToInt = toInteger.digitToInt

unsafeStringToInt :: String -> Integer
unsafeStringToInt s = read s :: Integer


-- returns the next valid unsigned integer from the input string
-- if it succeeds it returns (True, number, remaining-string)
-- otherwise it returns (False, 0, input-string)
-- remaining-string is the input string - the chars used to construct a number at the start of the string
stringToInt :: String -> (Bool, Integer, String)
stringToInt [] = (False, 0, "")
stringToInt [c]
 | isDigit c = (True, charToInt c, "")
 | otherwise = (False, 0, [c])
stringToInt (c:sx)
 | isDigit c && (isDigit.head) sx = do
    let s = takeWhile isDigit (c:sx)
    let x = unsafeStringToInt s
    if 0 <= x && x <= 999 then
        (True, x, drop (length s -1) sx)
    else
        (False, 0, c:sx)
 | isDigit c = (True, charToInt c, sx)
 | otherwise = (False, 0, c:sx)

-- unused as 0000 is indistinguishable from 0
base10Concat :: Integer -> Integer -> Integer
base10Concat a b = do
    let base = floor (logBase 10 (fromIntegral b))
    a * floor (10 ** fromIntegral (1+base)) + b

-- takes a prefix, input
-- if input starts with prefix
-- return true, input - leading prefix
-- otherwise false, input
stripPrefix :: String -> String -> (Bool, String)
stripPrefix prefix s
 | T.isPrefixOf (T.pack prefix) (T.pack s) = (True, drop (length prefix) s)
 | otherwise = (False, s)


-- takes "mul(22, 3)foo" returns (22*3, "foo")
mul :: String -> (Integer, String)
mul s = do
  let (a, sa) = stripPrefix "mul(" s
  let (b, i, sb) = stringToInt sa
  let (c, sc) = stripPrefix "," sb
  let (d, j, sd) = stringToInt sc
  let (e, se) = stripPrefix ")" sd
  if a && b && c && d && e then (i*j, se) else (0, s)

_part_1 :: Integer -> String -> Integer
_part_1 acc [] = acc
_part_1 acc (s:sx) = acc + (fst.mul) (s:sx) + _part_1 acc sx

part_1 = _part_1 0

disable :: String -> Bool
disable = fst.stripPrefix "don't()"

enable :: String -> Bool
enable = fst.stripPrefix "do()"

_part_2 :: Integer -> Bool -> String -> Integer
_part_2 acc en [] = acc
_part_2 acc en s
 | enable s = _part_2 acc True (tail s)
 | disable s = _part_2 acc False (tail s)
 | not en = _part_2 acc en (tail s)
 | otherwise = acc + (fst.mul) s + _part_2 acc en (tail s)

part_2 = _part_2 0 True

program :: String -> String
program s = "part 1: " ++ (show.part_1) s ++ "\n" ++
    "n>6289509 :: " ++ show (part_1 s > 6289509) ++ "\n" ++
    "n>120967239 :: " ++ show (part_1 s > 120967239) ++ "\n" ++
    "part 2: " ++ (show.part_2) s ++ "\n"

-- debug function
example :: String -> String
example [] = ""
example s = do
    let (n, sn) = mul s
    let input = take (length s - length sn) s
    if (not.null) input
    then input ++ " => " ++ show n ++ "\n" ++ example (tail s)
    else
        if n /= 0
        then "???"
        else
            example (tail s)

main :: IO ()
main = do
    let path = ".input"
    text <- readFile path
    let output = program text
    putStr output

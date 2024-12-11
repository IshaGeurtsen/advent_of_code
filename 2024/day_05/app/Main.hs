{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

indexOf :: String -> String -> Maybe Int
indexOf key "" = Nothing
indexOf key text =
  if indexOfMatch key text
    then Just 0
    else case indexOf key (tail text) of
      Nothing -> Nothing
      Just index -> Just (index + 1)

indexOfMatch :: String -> String -> Bool
indexOfMatch "" "" = True
indexOfMatch "" text = True
indexOfMatch key "" = False
indexOfMatch (k : key) (t : text) = k == t && indexOfMatch key text

split :: String -> String -> [String]
split sep "" = []
split seperator text = do
  case indexOf seperator text of
    Nothing -> [text]
    Just index -> take index text : split seperator (drop (index + length seperator) text)

program :: String -> String
program input = do
  let inputParts = split "\n\n" input
  let pageOrderRuleText = head inputParts
  let updatePageList = (head . tail) inputParts
  "hello Haskell" ++ updatePageList

testCase :: (Eq a, Show a) => a -> a -> String -> Maybe String
testCase output expected error =
  if output == expected
    then Nothing
    else Just (error ++ " :: " ++ show output ++ " /= " ++ show expected) :: Maybe String

tests :: String -> String
tests input = do
  let results =
        [ testCase (indexOf "b" "abc") (Just 1) "indexOf middle should return middle",
          testCase (indexOf "d" "abc") Nothing "indexOf missing should return Nothing",
          testCase (indexOf "b" "aaabccc") (Just 3) "indexOf long middle should return middle",
          testCase (split "b" "abc") ["a", "c"] "split sep in middle should return head and tail skipping seperator",
          testCase (split "d" "abc") ["abc"] "split with missing seperator should return original",
          testCase (split "b" "aaabccc") ["aaa", "ccc"] "split returns entire head and entire tail",
          testCase (split "b" "aaabbccc") ["aaa", "", "ccc"] "split double seperator should return empty string"
        ] ::
          [Maybe String]
  let testCount = length results
  if all testPass results
    then "all " ++ show testCount ++ " tests pass"
    else head (foldr1 (++) (map testFail results))

testPass :: Maybe String -> Bool
testPass a = case a of
  Nothing -> True
  Just b -> False

testFail :: Maybe String -> [String]
testFail a = case a of
  Nothing -> []
  Just b -> [b]

main :: IO ()
main = do
  text <- readFile ".input"
  (putStrLn . program) text
  (putStrLn . tests) text

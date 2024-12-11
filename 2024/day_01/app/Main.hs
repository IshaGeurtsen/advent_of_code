toPair :: String -> (Integer, Integer)
toPair s = (0, 0)

toList :: String -> [(Integer, Integer)]
toList s = map toPair (lines s)

firstList :: String -> [Integer]
-- firstList = fst . head . toList
firstList s = []

secondList :: String -> [Integer]
-- secondList = snd . toList
secondList s = []

unsafeStringToInt :: String -> Integer
unsafeStringToInt s = read s :: Integer

program :: String -> String
-- program s = map unsafeStringToInt ((words . head . lines) s)
program s = s

main = do
  text <- readFile ".input"
  (putStr . program) text

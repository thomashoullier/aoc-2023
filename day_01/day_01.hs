-- Run in the REPL.
import Data.Char

-- # PART 1 #
-- Parse a string, return all digits in a list.
stringToDigits :: String -> [Char]
stringToDigits = filter isDigit

-- Creates an integer from the first and last digit in a list.
numFromFirstLast :: [Char] -> Int
numFromFirstLast digits = read [head digits, last digits]

-- Find all such numbers in a set of lines
numsInLines :: [String] -> [Int]
numsInLines lines = map (numFromFirstLast . stringToDigits) lines

main = do putStrLn "# Day 01 #"
          contents <- readFile "input.txt"
          let ls = lines contents
              res = (sum . numsInLines) ls
          print res

oneliner = readFile "input.txt" >>= print . sum . map (read . (\xs -> [head xs, last xs]) . filter isDigit) . lines

-- # PART 2 #

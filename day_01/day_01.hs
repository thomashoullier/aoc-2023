-- Run in the REPL.
import Data.Char
import Data.String.Utils -- Requires the package MissingH

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

-- # PART 1 with fewer code #
part1_fun = read . (\xs -> [head xs, last xs]) . filter isDigit
part1_res = readFile "input.txt" >>= print . sum . map part1_fun . lines

-- # PART 2 #
-- We simply start by replacing all instances of digits as letters
-- and then apply the same part 1 function again.

-- Note that "oneight" counts as "18"

-- We have to put the letters back to the right and left because they can be
-- used to form another digit.

-- It would have been better to have a "findfirst" and "findlast" digit
-- operating on each string.

digitsTable :: [(String, String)]
digitsTable = [("one", "one1one"),
               ("two", "two2two"),
               ("three", "three3three"),
               ("four", "four4four"),
               ("five", "five5five"),
               ("six", "six6six"),
               ("seven", "seven7seven"),
               ("eight", "eight8eight"),
               ("nine", "nine9nine")]

-- List of replacement functions, one for each number.
replacements = [replace m r | (m, r) <- digitsTable]
-- modLine applies all the replacements to a given line
modLine :: String -> String
modLine line = foldl (\x f -> f x) line replacements

part2_res = readFile "input.txt" >>= print . sum .
  map (part1_fun . modLine) . lines

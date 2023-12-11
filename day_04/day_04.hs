-- Day 04

import Data.Char (isDigit)
import Data.List.Split (splitOneOf, splitOn)
import Data.List (intersect)

ex_input =
     "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n"
  ++ "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n"
  ++ "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n"
  ++ "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n"
  ++ "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n"
  ++ "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11\n"

ex_line = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"

-- * Parse the problem

-- cardId, winning number, numbers you have
type Card = (Int, ([Int], [Int]))

id :: Card -> Int
id = fst

winN :: Card -> [Int]
winN = fst . snd

havN :: Card -> [Int]
havN = snd . snd

-- Split a problem string into the three strings containing the card id,
-- the winning numbers and the numbers you have.
splitCardStr :: String -> [String]
splitCardStr = splitOneOf ":|"
-- ex: splitCardStr ex_line -> ["Card 1", " 41 48 ..", " 83 86 .."]

-- Parse the Id of a CardId string
parseId :: String -> Int
parseId = read . filter isDigit
-- ex: parseId "Card 1" -> 1

-- Parse a list of numbers
parseN :: String -> [Int]
parseN = map read . filter (not . null) . splitOn " "

-- Card parser
parseCard :: String -> Card
parseCard s = (parseId idStr,
               (parseN winStr, parseN havStr))
  where [idStr, winStr, havStr] = splitCardStr s
-- ex: parseCard ex_line

-- * Part 1

-- Count the numbers in common between two lists
countMatches :: [Int] -> [Int] -> Int
countMatches xs ys = length $ intersect xs ys

-- Convert a number of matches to a number of points.
matchToPoints :: Int -> Int
matchToPoints 0 = 0
matchToPoints m = 2 ^ (m - 1)

-- Count the number of points a Card is worth
points :: Card -> Int
points c = matchToPoints nMatches
  where nMatches = countMatches (winN c) (havN c)

-- Parse a stack of Cards, count their points and sum them.
pointsStack :: [String] -> Int
pointsStack = sum . map (points . parseCard)

part1 = do putStrLn "# Part 1 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               res = pointsStack ls
           print res

-- * Part 2
-- We track the number of instances of each card in a list.
-- We add a list of ones*instances of size nMatches to every next position in
-- this list, starting from the beginning.

-- Sum two lists of equal size with: zipWith (+) l1 l2
-- we need to zero pad the ones*instances
addList :: [Int] -> [Int] -> [Int]
addList = zipWith (+)

-- zero-pad a list by placing it at a given index in the padded list.
-- We do not account for going over the specified length.
-- length ind list
zeroPad :: Int -> Int -> [Int] -> [Int]
zeroPad l i xs = (replicate i 0) ++ xs ++ (replicate (l - il - i) 0)
  where il = length xs

-- Run the algorithm over the Cards, getting the number of instances
-- per Card. We proceed by recursive iteration.
propagCardsH :: [Card] -> [Int] -> [Int]
propagCardsH [c] [ns] = [ns] -- The number of instances is not modified.
propagCardsH (c:cs) (n:ns) = [n] ++ (propagCardsH cs (addList newNums ns))
  where matches = countMatches (winN c) (havN c)
        l = length ns
        newNums = zeroPad l 0 (replicate matches n)
-- propagCardsH cards [1,1,1,..]

propagCards :: [Card] -> [Int]
propagCards cs = propagCardsH cs (replicate l 1)
  where l = length cs
-- ex: propagCards $ map parseCard $ lines ex_input -> [1,2,4,8,14,1]

part2 = do putStrLn "# Part 2 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               cs = map parseCard ls
               ns = propagCards cs
               res = sum ns
           print res

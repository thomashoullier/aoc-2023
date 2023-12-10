-- Day 04

import Data.Char (isDigit)
import Data.List.Split (splitOneOf, splitOn)

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

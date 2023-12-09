-- Day 02

import Data.Char (isDigit)
import Data.List.Split

ex_input = "467..114..\n"
        ++ "...*......\n"
        ++ "..35..633.\n"
        ++ "......#...\n"
        ++ "617*......\n"
        ++ ".....+.58.\n"
        ++ "..592.....\n"
        ++ "......755.\n"
        ++ "...$.*....\n"
        ++ ".664.598..\n"

-- We proceed as follows. We go over the set of lines by packs of three.
-- The pack has a center line, a top line, and a bottom line.
-- We detect consecutive digits in the center line, and store the
-- characters in a new pack, cutting out the digits with their adjacent
-- characters.
-- eg.
--       ..*.
--       .35.
--       ....
-- We can then parse the number and detect the symbol easily.

-- * First we try doing this on a single line
ex_line = ".14*....223....2"

-- Return a string padded with '.' on both sides
padString :: String -> String
padString s = ['.'] ++ s ++ ['.']

detNum :: String -> String
detNum s = [b | (a, b, c) <- zip3 s (drop 1 s) (drop 2 s),
                or [isDigit b, isDigit a, isDigit c]]

-- ex: detNum $ padString ex_line
-- ".14*.223..2"

cutNumH :: String -> String -> [String]
cutNumH [c] [] = []
cutNumH (c:s) [] = cutNumH s [c]
cutNumH [c] acc
  | isDigit c = [acc ++ [c]]
  | isDigit (last acc) = [acc]
  | otherwise = []
cutNumH (c:s) acc
  | isDigit c = cutNumH s (acc ++ [c])
  | isDigit (last acc) = [acc ++ [c]] ++ cutNumH s []
  | otherwise = cutNumH s [c]

cutNum :: String -> [String]
cutNum s = cutNumH s []

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
ex_line1 = "14*....223.2."
ex_line2 = "14*....223.2"
ex_line3 = ".14*....223.2."
ex_line4 = "..14*....223.2."

cutNumH :: String -> String -> [String]
cutNumH (c:s) [] = cutNumH s [c]
cutNumH [c] acc
  | isDigit c = [acc ++ [c]]
  | isDigit (last acc) = [acc ++ [c]]
  | otherwise = []
cutNumH (c:s) acc
  | isDigit c = cutNumH s (acc ++ [c])
  | isDigit (last acc) = [acc ++ [c]] ++ cutNumH s [c]
  | otherwise = cutNumH s [c]

cutNum :: String -> [String]
cutNum s = cutNumH s []

-- ex: cutNum ex_line1
-- ["14*",".223.",".2."]

-- * Now let's do the same thing on columns of characters.
ex_three = ("...*......",
            "..35..633.",
            "......#...")

type LineTriple = (String, String, String)
type Col = (Char, Char, Char) -- top, center, bottom

-- Access top, center and bottom element of a tuple
top (a, _, _) = a
cen (_, a, _) = a
bot (_, _, a) = a

-- Parse three strings into a list of Col
parseThree :: LineTriple -> [Col]
parseThree s = zip3 (top s) (cen s) (bot s)

type Block = [Col]

-- Get a row of a block as a string
getRow :: (Col -> Char) -> Block -> String
getRow acce b = map acce b
-- ex: getRow top $ parseThree ex_three

-- Block printer for debug
strBlock :: Block -> String
strBlock b = (getRow top b) ++ "\n"
            ++ (getRow cen b) ++ "\n"
            ++ (getRow bot b) ++ "\n"
printBlock b = putStrLn $ strBlock b
printBlocks bs = sequence (map printBlock bs)

-- cutNumH but with columns
cutNumHC :: [Col] -> [Col] -> [Block]
cutNumHC (c:s) [] = cutNumHC s [c]
cutNumHC [c] acc
  | isDigit (cen c) = [acc ++ [c]]
  | isDigit (cen (last acc)) = [acc ++ [c]]
  | otherwise = []
cutNumHC (c:s) acc
  | isDigit (cen c) = cutNumHC s (acc ++ [c])
  | isDigit (cen (last acc)) = [acc ++ [c]] ++ cutNumHC s [c]
  | otherwise = cutNumHC s [c]

cutNumC :: [Col] -> [Block]
cutNumC cols = cutNumHC cols []

-- ex: printBlocks $ cutNumC $ parseThree ex_three

-- * Now feed an input and return the blocks.

-- We need to pad the first and last line of the input first.
dottedStr :: String -> String
dottedStr s = replicate (length s) '.'

padDot :: [String] -> [String]
padDot ss = [dots] ++ ss ++ [dots]
  where dots = dottedStr (head ss)
-- ex: padDot $ lines ex_input

-- Create LineTriple from the input
parseInThree :: [String] -> [LineTriple]
parseInThree ss = [lts | lts <- zip3 ss (drop 1 ss) (drop 2 ss)]
-- ex: parseInThree $ padDot $ lines ex_input

-- Return detected blocks from input.
getBlocks :: [String] -> [Block]
getBlocks ss = concat $ map (cutNumC . parseThree)
                            (parseInThree $ padDot ss)
-- ex: printBlocks $ getBlocks $ lines ex_input

-- * Is there a symbol anywhere in a Block?

-- Get all the characters from a block as a single string
getChars :: Block -> String
getChars b = (getRow top b) ++ (getRow cen b) ++ (getRow bot b)

-- Symbol predicate: is there a symbol in the block?
symInBlockP :: Block -> Bool
symInBlockP b = not $ null $ filter (\c -> (not $ isDigit c) && c /= '.')
                                    (getChars b)

-- * Parse the number in the block
numInBlock :: Block -> Int
numInBlock b = read $ filter isDigit $ getRow cen b

-- * Solution to part 1
partSum :: [String] -> Int
partSum ss = sum $ map numInBlock $ filter symInBlockP $ getBlocks ss
-- ex: partSum $ lines ex_input

part1 = do putStrLn "# Part 1 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               res = partSum ls
           print res

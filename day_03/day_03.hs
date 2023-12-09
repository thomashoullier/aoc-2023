-- Day 03
import Data.Char
import Data.List

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

-- 1. Parsing the input
-- We put the input in a matrix.
-- The matrix holds properties for each cell.

-- A cell can be either a symbol, a dot or a digit.
data CellType = Sym | Dot | Dig

-- Equality predicate with Dig
digP :: CellType -> Bool
digP Dig = True
digP _ = False


data Cell = Cell {char :: Char, cellType :: CellType}
type Matrix = [[Cell]]

-- Printers for debugging
printCellType :: CellType -> Char
printCellType Sym = 'S'
printCellType Dot = '.'
printCellType Dig = 'D'

cellt :: Cell -> Char
cellt c = printCellType $ cellType c

rowStr :: (Cell -> Char) -> [Cell] -> String
rowStr acc r = map acc r

matrixStr :: (Cell -> Char) -> Matrix -> [String]
matrixStr acc m = map (rowStr acc) m

printMatrix :: (Cell -> Char) -> Matrix -> IO ()
printMatrix acc m = putStrLn $ unlines $ matrixStr acc m
-- ex: printMatrix char m
--   : printMatrix cellt m

-- Parse a character into a CellType
parseType :: Char -> CellType
parseType c
  | isDigit c = Dig
  | c == '.'  = Dot
  | otherwise = Sym

-- Parse a single character into a Cell.
parseCell :: Char -> Cell
parseCell c = Cell {char = c, cellType = parseType c}

-- Parse a single row
parseRow :: String -> [Cell]
parseRow r = map parseCell r

-- Parse the input string into a Matrix
strToMatrix :: String -> Matrix
strToMatrix s = map parseRow (lines s)
-- ex: strToMatrix ex_input

-- 2. Part 1
-- Our plan is to store every number into a separate table.
-- This table stores the numbers, and it also holds properties
-- such as adjacency with a symbol.
-- The matrix holds an index into the table of digits at these
-- cells that contain a digit.
-- We mark every symbol in the matrix with a boolean in the cell
-- property.
-- We iterate through every symbol and check for adjacent numbers.
-- We mark every number found in the table.

type Pos = (Int, Int) -- Position in the matrix.

-- Cell accessor by Pos in Matrix
getCell :: Matrix -> Pos -> Cell
getCell m p = m !! (fst p) !! (snd p) -- terrible :(

--- First we find the Pos of every digit in the matrix, by group
--- of adjacent digits.

-- Is a Cell of CellType Dig?
cellIsDigit :: Cell -> Bool
cellIsDigit c = digP $ cellType c

-- Find the index of digits in a row of Cell.
digInd :: [Cell] -> [Int]
digInd cs = findIndices cellIsDigit cs

-- Credit to https://gitlab.haskell.org/ghc/ghc/-/issues/1408
groupWhen :: (a -> a -> Bool) -> [a] -> [[a]]
groupWhen _ []    = []
groupWhen _ [a]   = [[a]]
groupWhen f (a:l) = if f a (head c) then (a:c):r
                                    else [a]:c:r
  where (c:r) = groupWhen f l

-- Group the consecutive integers together in a sorted list of integers.
groupConsec :: [Int] -> [[Int]]
groupConsec = groupWhen (\x y -> y - x == 1)

-- Given a row index and a list of integer groups, convert to Pos
toPos :: Int -> [[Int]] -> [[Pos]]
toPos rowInd inds = map (\g -> map (\x -> (rowInd, x)) g) inds

-- ex:
-- m1 = strToMatrix ex_input
-- groupConsec $ digInd $ head m1
-- [[0,1,2],[5,6,7]]
-- toPos 2 (groupConsec $ digInd $ head m1)
-- [[(2,0),(2,1),(2,2)],[(2,5),(2,6),(2,7)]]

-- Return the groups of digits in the matrix by their position.
digPos :: Matrix -> [[Pos]]
digPos m = concat [toPos irow (groupConsec $ digInd row)
           | (row, irow) <- zip m [0 .. (length m) - 1]]

--- Then we parse the numbers indicated by these digits, assign them
--- an index and store them in the numbers table.

-- Convert a group of positions of consecutive characters to the corresponding
-- integer.
groupToNum :: Matrix -> [Pos] -> Int
groupToNum m ps = read $ map (char . getCell m) ps
-- ex groupToNum m [(0,0),(0,1),(0,2)] -> 467

-- Is there a Sym adjacent to a Pos?

-- Now we iterate over the groups of numbers in the Matrix, we:
-- - Read the Int, store it in the NumeTab
-- - Store a reference to the NumeTab entry in every digit Cell (maybe use
--   another matrix idk)

-- A number stores its value and whether it is adjacent to a symbol.
data Nume = Nume {val :: Int, adj :: Bool}

instance Show Nume where
  show (Nume v a) = "Nume {val: " ++ show v ++ ", adj: " ++ show a ++ "}"

-- Table of Nume
type NumeTab = [Nume]

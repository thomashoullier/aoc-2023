-- Day 03
import Data.Char

-- 1. Parsing the input
-- We put the input in a matrix.
-- The matrix holds properties for each cell.

-- A cell can be either a symbol, a dot or a digit.
data CellType = Sym | Dot | Dig
data Cell = Cell {char :: Char, cellType :: CellType}
type Matrix = [[Cell]]

-- Printers for debugging
printCellType :: CellType -> String
printCellType Sym = "S"
printCellType Dot = "."
printCellType Dig = "D"

rowStr :: [Cell] -> String
rowStr r = map char r

matrixStr :: Matrix -> [String]
matrixStr m = map rowStr m

-- TODO: add an argument to print either the char or the cellType
printMatrix :: Matrix -> IO ()
printMatrix m = putStrLn $ unlines $ matrixStr m

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

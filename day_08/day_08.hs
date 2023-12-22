-- Day 08
import qualified Data.Map
import Data.List.Split (splitOn, splitWhen)
import Data.Char (isLetter)
import Data.Maybe (fromJust)
import qualified Data.CircularList

ex_input1 = "RL\n"
         ++ "\n"
         ++ "AAA = (BBB, CCC)\n"
         ++ "BBB = (DDD, EEE)\n"
         ++ "CCC = (ZZZ, GGG)\n"
         ++ "DDD = (DDD, DDD)\n"
         ++ "EEE = (EEE, EEE)\n"
         ++ "GGG = (GGG, GGG)\n"
         ++ "ZZZ = (ZZZ, ZZZ)\n"

ex_input2 = "LLR\n"
         ++ "\n"
         ++ "AAA = (BBB, BBB)\n"
         ++ "BBB = (AAA, ZZZ)\n"
         ++ "ZZZ = (ZZZ, ZZZ)\n"

-- We can store the problem structure in a Map.
type Loc = String
type Fork = (Loc, Loc)
left = fst
right = snd
type Map = Data.Map.Map Loc Fork

ex = Data.Map.lookup "AAA" $ Data.Map.insert "AAA" "BBB" Data.Map.empty

-- Parse a map line into its Loc elements.
parseLine :: String -> (Loc, Loc, Loc)
parseLine s = (key, l, r)
  where [k, f] = splitOn "=" s
        key = filter isLetter k
        [l, r] = map (filter isLetter) $ splitOn "," f

-- Add the locations into the Map
addLocs :: Map -> (Loc, Loc, Loc) -> Map
addLocs m (k, l, r) = Data.Map.insert k (l,r) m

-- Create a map from a list of raw entries
mapEntries :: Map -> [(Loc, Loc, Loc)] -> Map
mapEntries m [e] = addLocs m e
mapEntries m (e:es) = mapEntries (addLocs m e) es

ex_mapStr = "AAA = (BBB, BBB)\n"
         ++ "BBB = (AAA, ZZZ)\n"
         ++ "ZZZ = (ZZZ, ZZZ)\n"

-- Create a whole map from the map block in the input
parseMap :: [String] -> Map
parseMap ss = mapEntries Data.Map.empty (map parseLine ss)
-- ex: parseMap $ lines ex_mapStr

ex_map = parseMap $ lines ex_mapStr

-- Directions: L or R
data Dir = L | R deriving (Show, Eq)
type Dirs = [Dir]

ex_dirStr = "LLR"

parseDir :: Char -> Dir
parseDir 'L' = L
parseDir 'R' = R

parseDirs :: String -> Dirs
parseDirs s = map parseDir s
-- ex: parseDirs ex_dirStr

-- Iterate one step through the Map
next :: Map -> Loc -> Dir -> Loc
next m k d
  | d == L = left e
  | d == R = right e
    where e = fromJust $ Data.Map.lookup k m

-- Parse the input into the directions and map
parseInput :: [String] -> (Dirs, Map)
parseInput ss = (parseDirs $ head dirStr, parseMap mapStr)
   where [dirStr, mapStr] = splitWhen (== "") ss
-- ex: parseInput $ lines ex_input1

(ex_dirs1, ex_maps1) = parseInput $ lines ex_input1
(ex_dirs2, ex_maps2) = parseInput $ lines ex_input2

-- Iterate through the map until reaching "ZZZ"
iterMap :: Map -> Dirs -> Loc -> Int -> Int
iterMap m (d:ds) l n
  | nl == "ZZZ" = nn
  | otherwise = iterMap m ds nl nn
    where nl = next m l d
          nn = n + 1
-- ex: iterMap ex_maps1 ex_dirs1 "AAA" 0

-- We need to make the directions into an infinite list.
a = Data.CircularList.fromList [1,2,3]
b = Data.CircularList.toInfList a

-- Create an infinite list for the direction list.
toInf :: Dirs -> Dirs
toInf ds = Data.CircularList.toInfList $ Data.CircularList.fromList ds

-- countIter
countIter :: Map -> Dirs -> Int
countIter m ds = iterMap m infDs "AAA" 0
  where infDs = toInf ds
-- ex: countIter ex_maps1 ex_dirs1

part1 = do putStrLn "# Part 1 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               (dirs, maps) = parseInput ls
               res = countIter maps dirs
           print res

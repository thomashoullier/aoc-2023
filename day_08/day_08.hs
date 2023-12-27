-- Day 08
import qualified Data.Map
import Data.List.Split (splitOn, splitWhen)
import Data.Char (isLetter)
import Data.Maybe (fromJust)
import qualified Data.CircularList
import qualified Data.HashSet as HashSet
import qualified Data.HashMap.Strict as HashMap
import Math.NumberTheory.Moduli.Chinese

-- * Examples for both parts
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

ex_input3 = "LR\n"
         ++ "\n"
         ++ "AAA = (CCE, XXX)\n"
         ++ "CCE = (CCK, CCK)\n"
         ++ "CCK = (AAB, AAB)\n"
         ++ "AAB = (AAZ, AAZ)\n"
         ++ "AAZ = (AAB, XXX)\n"
         ++ "BBA = (BBB, XXX)\n"
         ++ "BBB = (BBC, BBC)\n"
         ++ "BBC = (BBZ, BBZ)\n"
         ++ "BBZ = (BBB, BBB)\n"
         ++ "XXX = (XXX, XXX)\n"

-- Cycle starting from BBA:
-- BBA 1
-- BBB 2 <- Same State
-- BBC 1
-- BBZ 2
-- BBB 1
-- BBC 2
-- BBZ 1
-- BBB 2 <- Same State

-- Cycle starting from AAA:
-- AAA 1
-- CCE 2
-- CCK 1
-- AAB 2 <- Same State
-- AAZ 1
-- AAB 2 <- Same State

-- 1: -*****o ; n = 6, zi = 3
-- 2: ---*o   ; n = 2, zi = 1
-- Result: 6

-- * Example 4
-- We want the following structure:
-- 1: --*****o         ; n = 6, zi = 4
-- 2: ---*******o*     ; n = 9, zi = 7
-- crt [(4,6),(7,9)] = 16
-- Res = 16 + 3 = 19

ex_input4 = "LLL\n"
         ++ "\n"
         ++ "AAA = (BBB, XXX)\n"
         ++ "BBB = (CCC, XXX)\n"
         ++ "CCC = (DDD, XXX)\n"
         ++ "DDD = (EEE, XXX)\n"
         ++ "EEE = (FFF, XXX)\n"
         ++ "FFF = (GGG, XXX)\n"
         ++ "GGG = (HHZ, XXX)\n"
         ++ "HHZ = (CCC, XXX)\n"
         ++ "HHA = (III, XXX)\n"
         ++ "III = (JJJ, XXX)\n"
         ++ "JJJ = (KKK, XXX)\n"
         ++ "KKK = (LLL, XXX)\n"
         ++ "LLL = (MMM, XXX)\n"
         ++ "MMM = (NNN, XXX)\n"
         ++ "NNN = (OOO, XXX)\n"
         ++ "OOO = (PPP, XXX)\n"
         ++ "PPP = (QQQ, XXX)\n"
         ++ "QQQ = (RRZ, XXX)\n"
         ++ "RRZ = (SSS, XXX)\n"
         ++ "SSS = (KKK, XXX)\n"

-- 1:
-- AAA 1
-- BBB 2
-- CCC 3 <- Same State
-- DDD 1
-- EEE 2
-- FFF 3
-- GGG 1
-- HHZ 2
-- CCC 3 <- Same State

-- 2:
-- HHA 1
-- III 2
-- JJJ 3
-- KKK 1 <- Same State
-- LLL 2
-- MMM 3
-- NNN 1
-- OOO 2
-- PPP 3
-- QQQ 1
-- RRZ 2
-- SSS 3
-- KKK 1 <- Same State

-- * Part 1
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
(ex_dirs3, ex_maps3) = parseInput $ lines ex_input3

-- Iterate through the map until reaching "ZZZ"
iterMap :: Map -> Dirs -> Loc -> Int -> Int
iterMap m (d:ds) l n
  | nl == "ZZZ" = nn
  | otherwise = iterMap m ds nl nn
    where nl = next m l d
          nn = n + 1
-- ex: iterMap ex_maps1 ex_dirs1 "AAA" 0

-- Create an infinite list for the direction list.
toInf :: [a] -> [a]
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

-- * Part 2
-- We naively iterate through the map using a list of Loc, stopping when we
-- reach a list of Loc all ending in Z.
-- -> The naive mapping takes too long to run.
-- We have to detect the length of the cycles for every element, and when the
-- cycle passes through a Z element, and then compute when they sync up on
-- elements ending with Z.

-- -- Predicates for the nodes
endsIn :: Char -> Loc -> Bool
endsIn c l = last l == c
-- -- ex: endsIn 'A' "ENA" -> True
-- --     endsIn 'A' "ENA" -> False

-- -- Get the starting locations from the map.
getStarts :: Map -> [Loc]
getStarts m = filter (endsIn 'A') ks where ks = Data.Map.keys m

-- -- Get the next iteration, with all items simultaneously
-- multiNext :: Map -> [Loc] -> Dir -> [Loc]
-- multiNext m ks d = map (\k -> next m k d) ks
-- -- ex: multiNext ex_map (getStarts ex_map) L

-- -- Iterate through the Map with simultaneous elements
-- multiIterMap :: Map -> Dirs -> [Loc] -> Int -> Int
-- multiIterMap m (d:ds) l n
--   | and (map (endsIn 'Z') nls) = nn
--   | otherwise = multiIterMap m ds nls nn
--     where nls = multiNext m l d
--           nn = n + 1
-- -- ex: multiIterMap ex_map (toInf ex_dirs1) (getStarts ex_map) 0

-- multiCountIter :: Map -> Dirs -> Int
-- multiCountIter m ds = multiIterMap m infDs starts 0
--   where infDs = toInf ds
--         starts = getStarts m
-- -- multiCountIter ex_maps3 ex_dirs3

-- part2 = do putStrLn "# Part 2 #"
--            contents <- readFile "input.txt"
--            let ls = lines contents
--                (dirs, maps) = parseInput ls
--                res = multiCountIter maps dirs
--            print res
-- -> This does not work, the number of iterations to perform is too great.

-- * Part 2 with smarter treatment.
-- We measure the length of every individual loop.
-- We list the indices where there are elements ending with Z in every loop.
-- The longest simultaneous loop is the lowest common multiple of the length
-- of individual loop. The coincidence of Z elements can happen between
-- iteration zero and this longest loop.
-- The cycles have length l_i.
-- The m elements of cycle i which end in Z have indices z_m^i
-- The step n at which every element ends in Z is congruent, for every i,
-- n = z_m^i [n_i]
-- This is the chinese remainder theorem.
-- We have to check whether there is a single z per loop or not, as it would
-- complicate the search if there were multiple numbers. We can maybe
-- construct the smallest congruent number in some way?
-- -> It appears there is only one element ending in z per loop, so we can
--    apply the chinese remainder theorem.

-- Detect the length of a loop.
-- We characterize the state by the current node and the direction index.
-- Once we encounter the same state twice, we know we have reached the end of a
-- loop.
-- We return its length.

type Pos = Int
type State = (Loc, Pos)

-- Compute the beginning and start of the shortest cycle starting at a given Loc
cycBegEndH :: Map -> Dirs -> [Int] -> Loc -> HashMap.HashMap State Int -> Int -> (Int, Int)
cycBegEndH m (d:ds) (i:is) l h n
  | HashMap.member s h = (fromJust $ HashMap.lookup s h, n)
  | otherwise = cycBegEndH m ds is nl nh (n + 1)
    where nl = next m l d
          s = (l, i)
          nh = HashMap.insert s n h

cycBegEnd :: Map -> Dirs -> Loc -> (Int,Int)
cycBegEnd m ds l = cycBegEndH m (toInf ds) (toInf [1 .. length ds]) l HashMap.empty 0

-- Compute the length of a cycle starting at a given Loc.
cycLenH :: Map -> Dirs -> [Int] -> Loc -> HashSet.HashSet State -> Int -> Int
cycLenH m (d:ds) (i:is) l h n
  | HashSet.member s h = (n + 1)
  | otherwise = cycLenH m ds is nl nh (n + 1)
    where nl = next m l d
          s = (l, i)
          nh = HashSet.insert s h
-- ex: cycLenH ex_maps3 (toInf ex_dirs3) (toInf [1 .. length ex_dirs3]) "BBA" HashSet.empty 0

cycLen :: Map -> Dirs -> Loc -> Int
cycLen m ds l = cycLenH m (toInf ds) (toInf [1 .. length ds]) l HashSet.empty 0
-- ex: cycLen ex_maps3 ex_dirs3 "BBA" -> 8

-- Get the list of successive Loc in a cycle
cycLoc :: Map -> Dirs -> Loc -> Int -> [Loc]
cycLoc m (d:ds) l 1 = [l]
cycLoc m (d:ds) l n = l : cycLoc m ds nl (n - 1)
  where nl = next m l d
-- ex: cycLoc ex_maps3 (toInf ex_dirs3) "BBA" 8

-- List the indices of elements ending in Z in a cycle.
zElems :: Map -> Dirs -> Loc -> Int -> [Int]
zElems m ds l ncyc = indices
  where (elems, indices) = unzip $ filter (\e -> endsIn 'Z' (fst e)) $ zip (cycLoc m ds l ncyc) [0 ..]
-- ex: zElems ex_maps3 (toInf ex_dirs3) "BBA" 8

-- * Printing for debugging
ex_starts4 = do putStrLn "# Input starts ex_input4 #"
                let ls = lines ex_input4
                    (dirs, maps) = parseInput ls
                    starts = getStarts maps
                    lens = map (\l -> cycLen maps dirs l) starts
                    begends = map (\l -> cycBegEnd maps dirs l) starts
                    ns = map nBegEnds begends
                    bmax = maxBeg begends
                    zs = map (\(s,l) -> zElems maps (toInf dirs) s l) (zip starts lens)
                    zsi = map (\z -> (last z) - bmax) zs
                    nz = zip zsi ns
                    rem = posCrt $ crtr nz
                    res = rem + bmax
                print starts
                print "begends of loops: "
                print begends
                print "Position of z elements: "
                print zs
                print "nz couples: "
                print nz
                print "Result: "
                print res

ex_starts = do putStrLn "# Input starts #"
               contents <- readFile "input.txt"
               let ls = lines contents
                   (dirs, maps) = parseInput ls
                   starts = getStarts maps
                   lens = map (\l -> cycLen maps dirs l) starts
                   begends = map (\l -> cycBegEnd maps dirs l) starts
                   ns = map nBegEnds begends
                   bmax = maxBeg begends
                   zs = map (\(s,l) -> zElems maps (toInf dirs) s l) (zip starts lens)
                   zsi = map (\z -> (last z) - bmax) zs
                   nz = zip zsi ns
                   rem = posCrt $ crtr nz
                   res = rem + bmax
               print starts
               print "begends of loops: "
               print begends
               print "Position of z elements: "
               print zs
               print "nz couples: "
               print nz
               print "Result: "
               print res

-- The different cycles are misaligned. Let
--   - the iterations outside of the cycle,
--   * the iterations within a cycle,
--   o the element ending in Z.
-- Let's represent two loops side by side:
-- 1: -----******o*  -> beg: 5, end: 12, n = 8, z = 11
-- 2: ----***o**     -> beg: 4, end: 9, n = 6, z = 7

-- The loops are unsynced, we sync them according to the longest acyclic part.
-- 1: .******o* -> z = 6
-- 2: ***o**    -> z = 2
-- We measure the distance of the z from the point where we start the last
-- cycle.

-- Then we solve the CRT with (z,n) couples: (6,8), (2,6) -> 14
-- The problem answer is 14 with the initial acyclic length added, 14 + 5 = 19

-- Second example:
-- 1: -****o*    -> beg: 1, end: 6, n = 6, z = 5
-- 2: ----*o**   -> beg: 4, end: 7, n = 4, z = 5
-- The longest acyclic part is 4 in length.
-- 1: zi = 1, zn = (1,6)
-- 2: zi = 1, zn = (1,4)
-- crt [(1,6),(1,4)] -> (1,12) -> posCrt = 1
-- res = 1 + 4 = 5

-- * CRT
-- See Math.NumberTheory.Moduli.Chinese, chinese applied as a left-fold.

crt :: [(Int, Int)] -> (Int, Int)
crt digs = foldl (\a b -> fromJust $ chinese a b) (0,1) digs

crtr :: [(Int, Int)] -> (Int, Int)
crtr digs = foldr (\a b -> fromJust $ chinese a b) (0,1) digs

-- BUG: crt does not give the same result as crtr when working with the full
-- input, this is very puzzling to me.
-- The result is the same when we type foldr and foldl manually.
-- Is this a GHC problem?

-- The result returned by chinese is sometimes negative, we convert it to
-- positive.
posCrt :: (Int, Int) -> Int
posCrt (a, n)
  | a < 0 = a + n
  | otherwise = a
-- ex: posCrt $ crt [(0,3),(3,4),(4,5)] -> 39

-- Convert the beginning and end couples of cycles to lengths
nBegEnds :: (Int, Int) -> Int
nBegEnds (b, e) = e - b
-- nBegEnds (1,7) -> 6

-- Get the maximum beg number from beginning and end couples
maxBeg :: [(Int, Int)] -> Int
maxBeg begends = maximum $ map fst begends

-- Concatenating everything into a single callable function
part2_fun :: [String] -> Int
part2_fun ls = rem + bmax
  where (dirs, maps) = parseInput ls
        starts = getStarts maps
        lens = map (\l -> cycLen maps dirs l) starts
        begends = map (\l -> cycBegEnd maps dirs l) starts
        ns = map nBegEnds begends
        bmax = maxBeg begends
        zs = map (\(s,l) -> zElems maps (toInf dirs) s l) (zip starts lens)
        zsi = map (\z -> (last z) - bmax) zs
        nz = zip zsi ns
        rem = posCrt $ crtr nz
-- ex: part2_fun $ lines ex_input3

part2 = do putStrLn "# Part 2 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               res = part2_fun ls
           print res

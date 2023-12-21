-- Day 05
import Data.List (find, sort)
import Data.Maybe (fromMaybe, isNothing, fromJust)
import Data.List.Split (splitOn, splitWhen)

ex_input =
     "seeds: 79 14 55 13\n"
  ++ "\n"
  ++ "seed-to-soil map:\n"
  ++ "50 98 2\n"
  ++ "52 50 48\n"
  ++ "\n"
  ++ "soil-to-fertilizer map:\n"
  ++ "0 15 37\n"
  ++ "37 52 2\n"
  ++ "39 0 15\n"
  ++ "\n"
  ++ "fertilizer-to-water map:\n"
  ++ "49 53 8\n"
  ++ "0 11 42\n"
  ++ "42 0 7\n"
  ++ "57 7 4\n"
  ++ "\n"
  ++ "water-to-light map:\n"
  ++ "88 18 7\n"
  ++ "18 25 70\n"
  ++ "\n"
  ++ "light-to-temperature map:\n"
  ++ "45 77 23\n"
  ++ "81 45 19\n"
  ++ "68 64 13\n"
  ++ "\n"
  ++ "temperature-to-humidity map:\n"
  ++ "0 69 1\n"
  ++ "1 0 69\n"
  ++ "\n"
  ++ "humidity-to-location map:\n"
  ++ "60 56 37\n"
  ++ "56 93 4\n"

-- * Map entry type
data Entry = Entry { destStart :: Int, srcStart :: Int, rangeLen :: Int }
ex_entry = Entry { destStart = 50, srcStart = 98, rangeLen = 2 }
ex_entry2 = Entry { destStart = 52, srcStart = 50, rangeLen = 48 }

-- Identity Entry.
idEntry = Entry { destStart = 0, srcStart = 0, rangeLen = 0 }

-- Printer
instance Show Entry where
  show e = "Entry { destStart = " ++ show (destStart e) ++ ", srcStart = "
        ++ show (srcStart e) ++ ", rangeLen = " ++ show (rangeLen e) ++ " }"

-- Is a given requested number in the Entry range?
inEntry :: Entry -> Int -> Bool
inEntry e i = i >= start && i <= end
  where start = srcStart e
        end = start + (rangeLen e) - 1

-- Entry source to destination lookup.
-- It is assumed that the source is known to be in the entry.
lookEnt :: Entry -> Int -> Int
lookEnt e s = ds + (s - ss)
  where ss = srcStart e
        ds = destStart e

-- * Map type
type Map = [Entry]
ex_map = [ex_entry, ex_entry2]

-- Find the Entry which contains the source requested, if any. If none are
-- found, a default Entry is return for mapping the source to itself.
findEntry :: Map -> Int -> Entry
findEntry m n = fromMaybe idEntry e
  where e = find (\e -> inEntry e n) m

-- Map lookup.
lookMap :: Map -> Int -> Int
lookMap m n = lookEnt e n where e = findEntry m n
-- ex: lookMap ex_map 50 -> 52

-- * Parsing

-- Parse Entry
str_entry = "50 98 2"

parseEntry :: String -> Entry
parseEntry s = Entry ds ss rl where [ds, ss, rl] = map read (splitOn " " s)

-- Parse Map
str_map = ["seed-to-soil map:","50 98 2","52 50 48"]

parseMap :: [String] -> Map
parseMap ss = map parseEntry $ tail ss

-- Parse seeds
type Seed = Int
str_seeds = "seeds: 79 14 55 13"

parseSeeds :: String -> [Seed]
parseSeeds s = map read $ tail $ splitOn " " ss
  where ss = last $ splitOn ":" s

parseInput :: [String] -> ([Seed], [Map])
parseInput ss = (seeds, maps)
  where blocks = splitWhen (== "") ss
        seeds = parseSeeds $ head $ head blocks
        maps = map parseMap $ tail blocks
-- ex: parseInput $ lines ex_input

(ex_seeds, ex_maps) = parseInput $ lines ex_input

-- * Part 1

-- Apply all the maps in succession for lookups
lookMaps :: [Map] -> Seed -> Int
lookMaps [m] n = lookMap m n
lookMaps (m:ms) n = lookMaps ms (lookMap m n)
-- ex: lookMaps ex_maps 79

getLocs :: [Map] -> [Seed] -> [Int]
getLocs ms ss = map (lookMaps ms) ss
-- ex: getLocs ex_maps ex_seeds

minLoc :: [String] -> Int
minLoc ss = minimum $ getLocs maps seeds
  where (seeds, maps) = parseInput ss
-- ex: minLoc $ lines ex_input

part1 = do putStrLn "# Part 1 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               loc = minLoc ls
           print loc

-- * Part 2
-- We can try mapping in reverse the locations, starting from the lowest,
-- to find the first one which maps to any seed.
-- However, pay attention to the fact that the map in reverse is not so easy to
-- establish:
-- Example: if 2 is mapped to 4 in forward, 2 is mapped to 2 in reverse, which
--          is a contradiction. Maybe try seeing if the result is in the forward
--          map.
-- Another approach is to establish all the different range maps from seeds
-- to locations. Then we pick the map which includes the lowest location.

-- At a given level, the destination can either be mapped by an entry,
-- or mapped by default, or not mapped at all.
data MappingT = Mapped | Default | Unmapped deriving (Show, Eq)

whichMapping :: Map -> Int -> MappingT
whichMapping m n
  | inDestMap m n = Mapped
  | inSrcMap m n = Unmapped
  | otherwise = Default

-- Is a given requested Dest number in the Entry range?
inDestEntry :: Entry -> Int -> Bool
inDestEntry e i = i >= start && i <= end
  where start = destStart e
        end = start + (rangeLen e) - 1

inDestMap :: Map -> Int -> Bool
inDestMap m i = or (map (\e -> inDestEntry e i) m)

inSrcMap :: Map -> Int -> Bool
inSrcMap m i = or (map (\e -> inEntry e i) m)

-- At which upward distance is the given Dest number from changing mapping type?
-- If it is:
-- - Mapped: we look to the distance to the end of the current range.
-- - Unmapped: we look to the distance to the next destStart, or the end of the
--             current src range.
-- - Default: we look to the distance to the next destStart OR srcStart.
-- Infinite distances are represented by Nothing, Maybe Int

distNextMap :: Map -> Int -> Maybe Int
distNextMap m i
  | mapType == Mapped = distEntryEnd e i
  | mapType == Unmapped = minimumN [distNextDestStart m i, distSrcEnd se i]
  | mapType == Default = minimumN [distNextDestStart m i, distNextSrcStart m i]
    where mapType = whichMapping m i
          e = findDestEntry m i
          se = findSrcEntry m i

-- For a mapped destination number, find the corresponding Entry in the Map.
findDestEntry :: Map -> Int -> Maybe Entry
findDestEntry m i = find (\e -> inDestEntry e i) m

-- For an entry and a destination number, find the distance to the next mapping
-- type.
distEntryEnd :: Maybe Entry -> Int -> Maybe Int
distEntryEnd e i = Just (destEnd - i + 1)
  where destEnd = (destStart $ fromJust e) + (rangeLen $ fromJust e) - 1

-- Find the next destination range start, if any.
nextDestStart :: Map -> Int -> Maybe Int
nextDestStart m i = find (> i) destStarts
  where destStarts = sortedDestStarts m

-- Get the sorted list of the destination range starts.
sortedDestStarts :: Map -> [Int]
sortedDestStarts m = sort $ map destStart m

-- Distance to the next dest start in the map, if any.
distNextDestStart :: Map -> Int -> Maybe Int
distNextDestStart m i
  | isNothing nextStart = Nothing
  | otherwise = Just ((fromJust nextStart) - i)
    where nextStart = nextDestStart m i

-- Find the end of the src range the destination number is currently in.
distSrcEnd :: Maybe Entry -> Int -> Maybe Int
distSrcEnd e i = Just ((srcStart $ fromJust e) + (rangeLen $ fromJust e) - i)

-- Find the Entry which has the destination number mapped in its source range.
findSrcEntry :: Map -> Int -> Maybe Entry
findSrcEntry m i = find (\e -> inEntry e i) m

-- Distance to the next src start in the map, if any.
distNextSrcStart :: Map -> Int -> Maybe Int
distNextSrcStart m i
  | isNothing nextStart = Nothing
  | otherwise = Just ((fromJust nextStart) - i)
    where nextStart = nextSrcStart m i

nextSrcStart :: Map -> Int -> Maybe Int
nextSrcStart m i = find (> i) srcStarts
  where srcStarts = sortedSrcStarts m

sortedSrcStarts :: Map -> [Int]
sortedSrcStarts m = sort $ map srcStart m

-- Minimum function which maps Nothing to infinity.
minimumN :: [Maybe Int] -> Maybe Int
minimumN xs
  | null justL = Nothing
  | otherwise = minimum justL
    where justL = filter (not . isNothing) xs

-- * Now we just need to start at location 0, see how it is mapped through
--   the Maps and whether the eventual resulting seed is in a seed range.
--   We track the distance to the next range at each step in the maps,
--   including at the seed ranges stage.
--   We increment the location number by the shortest distance and
--   reevaluate the mapping from location to seeds.
--   We continue until we hit the first seed in a range.

-- Parse Seeds as ranges
data SeedR = SeedR { beg :: Int, len :: Int }

instance Show SeedR where
  show seed = "SeedR { beg = " ++ show (beg seed)
           ++ ", len = " ++ show (len seed) ++ " }"

parseSeedR :: String -> [SeedR]
parseSeedR s = parseSeedRH $ map read $ tail $ splitOn " " ss
  where ss = last $ splitOn ":" s
-- ex: parseSeedR str_seeds
ex_seedR = parseSeedR str_seeds

parseSeedRH :: [Int] -> [SeedR]
parseSeedRH [x,y] = [SeedR x y]
parseSeedRH (x:y:xs) = (SeedR x y) : parseSeedRH xs

-- Is a seed number in a given seed range?
inSeedRange :: SeedR -> Seed -> Bool
inSeedRange seedRange seed = seed >= start && seed <= end
  where start = beg seedRange
        end = start + (len seedRange) - 1

inSeeds :: [SeedR] -> Seed -> Bool
inSeeds seeds seed = or $ map (\r -> inSeedRange r seed) seeds

-- Distance to the next seed range, Nothing if infinite.
-- We only need the distance to the next range beginning.
distSeed :: [SeedR] -> Seed -> Maybe Int
distSeed seeds seed = find (> 0) dists
  where starts = map beg seeds
        dists = map (\x -> x - seed) $ sort $ starts

-- Map destination to source (reverse order)
mapToSrc :: Map -> Int -> Maybe Int
mapToSrc m i
  | mapT == Mapped = Just $ mapEntryToSrc e i
  | mapT == Unmapped = Nothing
  | mapT == Default = Just i
    where mapT = whichMapping m i
          e = findDestEntry m i

mapEntryToSrc :: Maybe Entry -> Int -> Int
mapEntryToSrc e i = sstart + dist
  where je = fromJust e
        start = destStart je
        dist = i - start
        sstart = srcStart je

-- Assess all the relevant data at a given Map stage
data Stage = Stage { src :: Maybe Int, mapT :: MappingT, dist :: Maybe Int }

instance Show Stage where
  show stage = "Stage { src = " ++ show (src stage)
            ++ ", mapT = " ++ show (mapT stage)
            ++ ", dist = " ++ show (dist stage) ++ "}"

getStage :: Map -> Int -> Stage
getStage m i = Stage {src = mapToSrc m i,
                      mapT = whichMapping m i,
                      dist = distNextMap m i}
-- ex: getStage ex_map 50

-- Propagate stages through the Maps starting at the first destination number.
getStages :: [Map] -> Int -> [Stage]
getStages [m] i = [getStage m i]
getStages (m:ms) i
  | (mapT stage == Unmapped) = [stage]
  | otherwise = stage : getStages ms (fromJust $ src stage)
    where stage = getStage m i
-- ex: getStages (reverse ex_maps) 46 -> 82

-- Get the lowest distance from a set of stages
lowestDist :: [Stage] -> Maybe Int
lowestDist stages = minimumN $ map dist stages

-- Iterate through location numbers until we hit the first seed
firstLoc :: [Map] -> [SeedR] -> Int -> Int
firstLoc maps seeds loc
  | stageT == Unmapped = firstLoc maps seeds nextLoc
  | inSeeds seeds (fromJust $ src lastStage) = loc
  | otherwise = firstLoc maps seeds nextLoc
    where stages = getStages maps loc
          lastStage = last stages
          stageT = mapT lastStage
          shortDist = lowestDist stages
          nextLoc = loc + (fromJust shortDist)
-- ex: firstLoc (reverse ex_maps) ex_seedR 0 -> 46

parseInput2 :: [String] -> ([SeedR], [Map])
parseInput2 ss = (seeds, maps)
  where blocks = splitWhen (== "") ss
        seeds = parseSeedR $ head $ head blocks
        maps = map parseMap $ tail blocks

part2 = do putStrLn "# Part 2 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               (seeds, maps) = parseInput2 ls
               loc = firstLoc (reverse maps) seeds 0
           print loc

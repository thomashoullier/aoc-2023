-- Day 06
import Data.List.Split (splitOn)

ex_input = "Time:      7  15   30\n"
        ++ "Distance:  9  40  200\n"

-- * Given the distances in play, we can simply compute the distance
--   given by all the possible durations of button-holding.

data Race = Race { time :: Int, distance :: Int }
instance Show Race where
  show race = "Race { time = " ++ show (time race)
           ++ ", distance = " ++ show (distance race) ++ " }"

ex_race = Race { time = 7, distance = 9 }

-- Compute the distance covered in given a total time and a holding time.
compDist :: Int -> Int -> Int
compDist t h = h * (t - h)

-- Compute all the possible distances for a given Race
allDists :: Race -> [Int]
allDists r = map (compDist t) [0 .. t]
  where t = time r

-- Count the number of different ways to beat the record for a given race
countWins :: Race -> Int
countWins r = length $ filter (> d) ds
  where ds = allDists r
        d = distance r

-- * Parse the input
parseNums :: String -> [Int]
parseNums s = map read $ filter (not . (== "")) $ splitOn " " ss
  where ss = last $ splitOn ":" s

parseInput :: [String] -> [Race]
parseInput ss = zipWith (\t d -> Race { time = t, distance = d }) times distances
  where times = parseNums $ head ss
        distances = parseNums $ last ss
-- ex: parseInput $ lines ex_input
ex_races = parseInput $ lines ex_input

-- * Compute part 1 result
countAllWins :: [Race] -> Int
countAllWins rs = product $ map countWins rs
-- ex: countAllWins ex_races

part1 = do putStrLn "# Part 1 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               races = parseInput ls
               res = countAllWins races
           print res

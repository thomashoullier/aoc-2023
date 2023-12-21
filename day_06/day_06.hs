-- Day 06
import Data.List.Split (splitOn)
import Data.Number.CReal

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

-- * Part 2
-- We now have to solve the problem analytically.
-- The distance covered is d = h * (t - h)
-- We need to count the number of d>dlim for h in [0, t]
-- d = dlim <-> dlim = -h**2 + ht <-> h**2 - ht + dlim = 0
-- There are two roots h1 and h2, h1 < h2, we just have to count the number of
-- integral steps between h1 and h2.
-- a = 1, b = -t, c = dlim
-- Delta = t**2 - 4 dlim
-- h1 = (t - sqrt(Delta)) / 2
-- h2 = (t + sqrt(Delta)) / 2
-- res = floor(h2) - ceil(h1) + 1

-- See the CReal type, we actually have to use it to avoid Haskell converting to
-- inexact floats.
compInterval :: Int -> Int -> Int
compInterval t d = (floor h2) - (ceiling h1) + 1
  where delta = t*t - 4 * d
        h1 = ((fromIntegral t) - ((sqrt :: CReal -> CReal) $ fromIntegral delta)) / (fromIntegral 2)
        h2 = ((fromIntegral t) + ((sqrt :: CReal -> CReal) $ fromIntegral delta)) / (fromIntegral 2)
-- ex: compInverval 7 9 -> 4

-- Parse the input in the new fashion
parseNum :: String -> Int
parseNum s = read $ concat $ filter (not . (== "")) $ splitOn " " ss
  where ss = last $ splitOn ":" s

parseInput2 :: [String] -> Race
parseInput2 ss = Race { time = t, distance = d }
  where t = parseNum $ head ss
        d = parseNum $ last ss
-- parseInput2 $ lines ex_input

ex_bigrace = parseInput2 $ lines ex_input

-- Count the number of different ways to win in a big Race
countBigWins :: Race -> Int
countBigWins r = compInterval t d
  where t = time r
        d = distance r
-- ex: countBigWins ex_bigrace

part2 = do putStrLn "# Part 2 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               race = parseInput2 ls
               res = countBigWins race
           print race
           print res

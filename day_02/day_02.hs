-- Day 02
import Data.Char
import Text.Regex.TDFA

data Round = Round { nRed :: Int, nGreen :: Int, nBlue :: Int }
data Game = Game { gid :: Int, rounds :: [Round] }

-- 1. Parsing the data
-- Read a count string like "3" or "" for zero.
readCount :: String -> Int
readCount "" = 0
readCount s = read s

matchRed = "[[:digit:]]+ red"
matchBlue = "[[:digit:]]+ blue"
matchGreen = "[[:digit:]]+ green"
matchNum = "[[:digit:]]+"

-- Given a color to match and a Round string, get the count for the cube
-- of the right color.
getColorCount :: String -> String -> Int
getColorCount matchStr s = readCount ((s =~ matchStr :: String) =~ matchNum)
-- ex. count = getColorCount matchBlue test_str

matchRound = "[^;]+"
matchRounds = ":.*$"

-- Get the list of Round strings from a Game String
getRounds :: String -> [String]
getRounds s = getAllTextMatches ((tail (s =~ matchRounds :: String))
                                 =~ matchRound)

-- Parse a Round string into a Round.
roundFromStr :: String -> Round
roundFromStr s = Round {nRed = getColorCount matchRed s,
                        nGreen = getColorCount matchGreen s,
                        nBlue = getColorCount matchBlue s}
-- ex: roundFromStr "3 blue, 4 red" -> Round{red: 4, green: 0, blue: 3}

-- Parse a Game id from string
matchId = "Game [[:digit:]]+"
gameId :: String -> Int
gameId s = read (filter isDigit (s =~ matchId :: String))

-- Parse a Game string into a Game.
gameFromStr :: String -> Game
gameFromStr s = Game{gid = gameId s,
                     rounds = map roundFromStr (getRounds s)}

-- 2. Solving part 1

-- The maximum possible counts are a kind of Round
maxRound = Round {nRed = 12, nGreen = 13, nBlue = 14}

-- Predicate for checking whether the round is possible or not.
rIsPossible :: Round -> Round -> Bool
rIsPossible maxr r = (nRed r) <= (nRed maxr)
                 && (nGreen r) <= (nGreen maxr)
                 && (nBlue r) <= (nBlue maxr)
-- ex. rIsPossible maxRound r -> True

-- Is a game possible?
gIsPossible :: Round -> Game -> Bool
gIsPossible maxr g = and (map (rIsPossible maxr) (rounds g))

part1 = do putStrLn "# Part 1 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               games = map gameFromStr ls
               pGames = [g | g <- games, gIsPossible maxRound g]
               pId = map gid pGames
           print (sum pId)

-- 3. Solving part 2

-- Find the minimal set of cubes for playing a list of Round.
minCubeSet :: [Round] -> Round
minCubeSet rs = Round{nRed = maximum (map nRed rs),
                      nGreen = maximum (map nGreen rs),
                      nBlue = maximum (map nBlue rs)}

-- Computing the power of a given Game
gPower :: Game -> Int
gPower g = (nRed minSet) * (nGreen minSet) * (nBlue minSet)
  where minSet = minCubeSet (rounds g)

part2 = do putStrLn "# Part 2 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               games = map gameFromStr ls
               res = sum (map gPower games)
           print res

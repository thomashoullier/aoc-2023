-- Day 07
import Data.List (group, sort, sortBy)
import Data.List.Split (splitOn)

ex_input = "32T3K 765\n"
        ++ "T55J5 684\n"
        ++ "KK677 28\n"
        ++ "KTJJT 220\n"
        ++ "QQQJA 483\n"

-- * Create a Card type with a comparison operator.
type Card = Char

-- We map every card to a number according to its ordering,
-- the least powerful card has number 1.
cardPow :: Card -> Int
cardPow '2' = 1
cardPow '3' = 2
cardPow '4' = 3
cardPow '5' = 4
cardPow '6' = 5
cardPow '7' = 6
cardPow '8' = 7
cardPow '9' = 8
cardPow 'T' = 9
cardPow 'J' = 10
cardPow 'Q' = 11
cardPow 'K' = 12
cardPow 'A' = 13

-- Ordering of cards (eg. for sortBy)
cardOrd :: Card -> Card -> Ordering
cardOrd c1 c2 = compare (cardPow c1) (cardPow c2)

-- * Hands
type Hand = [Card]

-- Recognize the type of Hand
data HandT = Five | Four | Full | Three | Two | One | High deriving (Show, Eq)

typeOfHand :: Hand -> HandT
typeOfHand h
  | gLen == 1 = Five
  | maxLen == 4 = Four
  | sort lengths == [2,3] = Full
  | maxLen == 3 = Three
  | sort lengths == [1,2,2] = Two
  | maxLen == 2 = One
  | otherwise = High
    where groups = group $ sort h
          gLen = length groups
          lengths = map length groups
          maxLen = maximum lengths
-- ex: typeOfHand "AAAAA" -> Five
--     typeOfHand "AA8AA" -> Four
--     typeOfHand "23332" -> Full
--     typeOfHand "TTT98" -> Three
--     typeOfHand "23432" -> Two
--     typeOfHand "A23A4" -> One
--     typeOfHand "23456" -> High

-- Map the types of Hand to a score for ranking
handPow :: HandT -> Int
handPow High = 1
handPow One = 2
handPow Two = 3
handPow Three = 4
handPow Full = 5
handPow Four = 6
handPow Five = 7

handScore :: Hand -> Int
handScore = handPow . typeOfHand

-- Compare the Hands first by their overall type
handOrd :: Hand -> Hand -> Ordering
handOrd h1 h2 = compare (handScore h1) (handScore h2)

-- Compare the Hands card by card
cardsComp :: [Card] -> [Card] -> Ordering
cardsComp [c1] [c2] = cardOrd c1 c2
cardsComp (c1:cs1) (c2:cs2)
  | o == EQ = cardsComp cs1 cs2
  | otherwise = o
    where o = cardOrd c1 c2

-- Hand ordering for sorting (sortBy)
handComp :: Hand -> Hand -> Ordering
handComp h1 h2
  | handO == EQ = cardsComp h1 h2
  | otherwise = handO
    where handO = handOrd h1 h2

-- * Parse the problem
type Bid = Int
type Entry = (Hand, Bid)

hand = fst
bid = snd

ex_entryStr = "32T3K 765"

parseEntry :: String -> Entry
parseEntry s = (h, read b)
  where [h,b] = splitOn " " s
-- ex: parseEntry ex_entryStr

parseInput :: [String] -> [Entry]
parseInput ss = map parseEntry ss
-- ex: parseInput $ lines ex_input

ex_entries = parseInput $ lines ex_input

entryComp :: Entry -> Entry -> Ordering
entryComp e1 e2 = handComp (hand e1) (hand e2)

sortEntries :: [Entry] -> [Entry]
sortEntries es = sortBy entryComp es

-- Compute the winnings from an unsorted set of entries
winnings :: [Entry] -> Int
winnings es = sum $ zipWith (\e r -> (bid e) * r) sortedE [1 .. len]
  where sortedE = sortEntries es
        len = length sortedE

part1 = do putStrLn "# Part 1 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               entries = parseInput ls
               res = winnings entries
           print res

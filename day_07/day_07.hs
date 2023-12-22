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

-- * Part 2
-- I believe we can simply group the Joker with the largest group of cards while
-- evaluating the type of Hand.

-- Let's rewrite everything with the new rules.
cardPow2 :: Card -> Int
cardPow2 'J' = 1
cardPow2 '2' = 2
cardPow2 '3' = 3
cardPow2 '4' = 4
cardPow2 '5' = 5
cardPow2 '6' = 6
cardPow2 '7' = 7
cardPow2 '8' = 8
cardPow2 '9' = 9
cardPow2 'T' = 10
cardPow2 'Q' = 11
cardPow2 'K' = 12
cardPow2 'A' = 13

-- Ordering of cards (eg. for sortBy)
cardOrd2 :: Card -> Card -> Ordering
cardOrd2 c1 c2 = compare (cardPow2 c1) (cardPow2 c2)

-- We change the way cards are grouped for counting.
groupHand :: Hand -> [Hand]
groupHand h = addToFront groups jokers
  where jokers = filter (== 'J') h
        groups = sortBy byLength $ group $ sort $ filter (not . (== 'J')) h

-- Sort by length predicate, longest group in front.
byLength :: Hand -> Hand -> Ordering
byLength h1 h2 = compare (length h2) (length h1)

-- Add a group to the front of groups
addToFront :: [Hand] -> Hand -> [Hand]
addToFront hs h = (h ++ head hs) : tail hs

typeOfHand2 :: Hand -> HandT
typeOfHand2 h
  | h == "JJJJJ" = Five
  | gLen == 1 = Five
  | maxLen == 4 = Four
  | sort lengths == [2,3] = Full
  | maxLen == 3 = Three
  | sort lengths == [1,2,2] = Two
  | maxLen == 2 = One
  | otherwise = High
    where groups = groupHand h
          gLen = length groups
          lengths = map length groups
          maxLen = maximum lengths
-- ex: typeOfHand2 "32T3K" -> One
--     typeOfHand2 "KK677" -> Two
--     typeOfHand2 "T55J5" -> Four
--     typeOfHand2 "KTJJT" -> Four
--     typeOfHand2 "QQQJA" -> Four

-- Compare the Hands card by card
cardsComp2 :: [Card] -> [Card] -> Ordering
cardsComp2 [c1] [c2] = cardOrd2 c1 c2
cardsComp2 (c1:cs1) (c2:cs2)
  | o == EQ = cardsComp2 cs1 cs2
  | otherwise = o
    where o = cardOrd2 c1 c2

-- Hand ordering for sorting (sortBy)
handComp2 :: Hand -> Hand -> Ordering
handComp2 h1 h2
  | handO == EQ = cardsComp2 h1 h2
  | otherwise = handO
    where handO = handOrd2 h1 h2

handScore2 :: Hand -> Int
handScore2 = handPow . typeOfHand2

-- Compare the Hands first by their overall type
handOrd2 :: Hand -> Hand -> Ordering
handOrd2 h1 h2 = compare (handScore2 h1) (handScore2 h2)

entryComp2 :: Entry -> Entry -> Ordering
entryComp2 e1 e2 = handComp2 (hand e1) (hand e2)

sortEntries2 :: [Entry] -> [Entry]
sortEntries2 es = sortBy entryComp2 es

-- Compute the winnings from an unsorted set of entries
winnings2 :: [Entry] -> Int
winnings2 es = sum $ zipWith (\e r -> (bid e) * r) sortedE [1 .. len]
  where sortedE = sortEntries2 es
        len = length sortedE
-- ex: winnings2 $ parseInput $ lines ex_input

part2 = do putStrLn "# Part 2 #"
           contents <- readFile "input.txt"
           let ls = lines contents
               entries = parseInput ls
               res = winnings2 entries
           print res

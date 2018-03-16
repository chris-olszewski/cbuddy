import Text.Read (readMaybe)
import Data.List (intersperse, group, sort, foldr1, nub, subsequences, (\\))

-- TODO: write tests

-- Basic Structure Stuff
data Suit = Hearts | Spades | Clubs | Diamonds deriving (Eq, Show)

data Card = Card { suit :: Suit
                 , rank :: Int
                 } deriving (Show, Eq)

data Hand = Hand { top :: Card
                 , rest :: [Card]
                 } deriving (Show)

fullDeck :: [Card]
fullDeck = [(Card s r) | s <- [Hearts, Spades, Clubs, Diamonds], r <- [1..13]]

-- Card Counting Stuff
count :: Hand -> Int
count h = sum $ [knobs, pairs, runs, fifteens, suits] <*> pure h

knobs :: Hand -> Int
knobs (Hand t r)
    | any (== target) r = 1
    | otherwise = 0
    where target = Card (suit t) 11

pairs :: Hand -> Int
pairs (Hand t r) = sum $ pairPoints (t : r) where
    pairPoints = (map rankPoints) . group . sort . (map rank)
    rankPoints = (*2) . ((flip choose) 2) . length
    choose n k = div (product [n-k+1..n]) (product [1..k])

-- TODO: clean up code
runs :: Hand -> Int
runs (Hand t r) = ((length . nub) run) * (product $ ((map length) . group) run) where
    run = (longestRun . sort . (map rank)) (t : r)
    longestRun = (foldr seqMax []) . (filter (validRun . nub)) . subsequences
    validRun cs = (length cs >= 3) && (cs == (take (length cs) (iterate succ (head cs))))
    seqMax a b = if length a > length b then a else b

-- TODO: clean up code & make more efficient
fifteens :: Hand -> Int
fifteens (Hand t r) = ((*2) . length . sums) (t : r) where
    sums = (filter ((== 15) . sum)) . (foldr buildSum [[]]) . (map ((min 10) . rank))
    buildSum r ss = ((filter (\x -> 15 >= (sum x))) . concat . (map (\s -> [r : s, s]))) ss

suits :: Hand -> Int
suits (Hand t r)
    | all (== bodySuit) (fs : rs) = 5
    | all (== bodySuit) rs = 4
    | otherwise = 0
    where (fs : rs) = map suit (t : r)
          bodySuit = head rs

-- Choice stuff
possibleCards :: [Card] -> [Card]
possibleCards = (\\) fullDeck

expectedVal :: (Fractional a) => [Card] -> [Card] -> a
expectedVal possible cs = (fromIntegral (sum (map (count . constructHands) possible))) / (fromIntegral (length possible)) where
    constructHands c = Hand c cs

optimalHand :: (Ord a, Fractional a) => [Card] -> ([Card], a)
optimalHand cs = foldr1 (\(a,b) (c,d) -> if b < d then (c,d) else (a,b)) handPoints where
    possibleHands = map (\x -> cs \\ [x]) cs
    handPoints = zip possibleHands (map (expectedVal (possibleCards cs)) possibleHands)

-- IO Stuff

-- probably expect SUITNUM seperated by space
-- e.g. D1 H11 for [(Diamonds, 1), (Hearts, 11)]
readCards :: String -> Maybe [Card]
readCards s = sequenceA (map readCard (words s))

readCard :: String -> Maybe Card
readCard (s:[]) = Nothing
readCard [] = Nothing
readCard (s:rk) = (\a b -> Card a b) <$> (charToSuit s) <*> rnk where
    rnk = readMaybe rk >>= validRank -- Make use of Maybe monad
    validRank n = if (1 <= n && n <= 13) then Just n else Nothing

charToSuit :: Char -> Maybe Suit
charToSuit 'H' = Just Hearts
charToSuit 'S' = Just Spades
charToSuit 'C' = Just Clubs
charToSuit 'D' = Just Diamonds
charToSuit _ = Nothing

printCard :: Card -> String
printCard (Card s r) = (suitName s) : (show r) where
    suitName Hearts = 'H'
    suitName Spades = 'S'
    suitName Clubs = 'C'
    suitName Diamonds = 'D'

printCards :: [Card] -> String
printCards = concat . (intersperse " ") . (map printCard)

printSuggestion :: (Show a, Fractional a) => ([Card], a) -> String
printSuggestion (cs, e) = (printCards cs) ++ "\nExpected Value: " ++ (show e)

maybePrint :: Maybe String -> IO ()
maybePrint (Just s) = putStrLn s
maybePrint Nothing = putStrLn "An invalid card was entered"

-- TODO: pretty up style
main :: IO ()
main = readCards <$> getLine >>=
        (return . (fmap optimalHand)) >>=
        (return . (fmap printSuggestion)) >>=
        maybePrint
        

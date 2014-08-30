import Data.Ord
import Data.List
import Data.Maybe

ranks = "23456789TJQKA"

data Card = Card { rank :: Int
                 , suit :: Char }

instance Eq Card where
    (Card r1 s1) == (Card r2 s2) = r1 == r2

instance Ord Card where
    compare = comparing rank

instance Show Card where
    show (Card r s) = [ranks !! (r - 2), s]

readCard :: String -> Card
readCard [r, s] = Card ((fromJust (elemIndex r ranks)) + 2) s

sortCards :: [Card] -> [Card]
sortCards = concat . sortBy (flip (comparing length)) . group . sortBy (flip compare)

level :: [Card] -> Int
level a = 10 - fromJust (elemIndex True levels) where
    levels = [ isRoyalFlush, isStraightFlush, isFourOfAKind, isFullHouse, isFlush
             , isStraight, isThreeOfAKind, isTwoPairs, isOnePair, isHighCard ]
    isRoyalFlush = isStraightFlush && rank (head a) == 14
    isStraightFlush = isFlush && isStraight
    isFourOfAKind = lenKind == 4
    isFullHouse = isThreeOfAKind && length (b !! 1) == 2
    isFlush = length (groupBy (\p q -> suit p == suit q) a) == 1
    isStraight = c == take 5 (iterate (\x -> x - 1) (head c)) where c = map rank a
    isThreeOfAKind = lenKind == 3
    isTwoPairs = isOnePair && length (b !! 1) == 2
    isOnePair = lenKind == 2
    isHighCard = True
    lenKind = length (head b)
    b = group a

score :: [Card] -> (Int, [Int])
score a = (level b, map rank b) where b = sortCards a

play :: [Card] -> [Card] -> Int
play a b = if score a > score b then 1
                                else 0

main = readFile "054.in" >>= 
    print . sum . map (uncurry play . splitAt 5 . map readCard . words) . lines

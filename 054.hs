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
level a = 10 - fromJust (elemIndex True (map ($ a) levels)) where
    levels = [ isRoyalFlush, isStraightFlush, isFourOfAKind, isFullHouse, isFlush
             , isStraight, isThreeOfAKind, isTwoPairs, isOnePair, isHighCard ]
    isRoyalFlush a = isFlush a && isStraight a && (rank (head a) == 14)
    isStraightFlush a = isFlush a && isStraight a
    isFourOfAKind = (== 4) . length . head . group
    isFullHouse a = (length (head b) == 3) && (length (last b) == 2) where b = group a
    isFlush = (== 1) . length . groupBy (\p q -> suit p == suit q)
    isStraight a = a == take 5 (iterate (\(Card r s) -> Card (r - 1) s) (head a))
    isThreeOfAKind = (== 3) . length . head . group
    isTwoPairs a = (length (head b) == 2) && (length (b !! 1) == 2) where b = group a
    isOnePair = (== 2) . length . head . group
    isHighCard a = True

score :: [Card] -> (Int, [Card])
score a = (level b, b) where b = sortCards a

play :: [Card] -> [Card] -> Int
play a b = if score a > score b then 1
                                else 0

main = readFile "poker.txt" >>= 
    print . sum . map (uncurry play . splitAt 5 . map readCard . words) . lines
import Data.Char
import Data.List.Split

isSquared :: Int -> Bool
isSquared n = m * m == n where m = (round .  sqrt . fromIntegral) n

isTriangle :: Int -> Bool
isTriangle n = isSquared (1 + 8 * n)

wordValue :: String -> Int
wordValue = sum . map charValue where charValue c = ord c - ord 'A' + 1

main = readFile "words.txt" >>= print . length . filter isTriangle .
                                map (wordValue . tail . init) . splitOn ","
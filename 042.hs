import Data.Char
import Data.List.Split

isSquared n = m * m == n where m = round $ sqrt $ fromIntegral n

isTriangle n = isSquared $ 1 + 8 * n

wordValue = sum . map (\c -> ord c - ord 'A' + 1)

main = readFile "words.txt" >>= print . length . filter isTriangle .
                                map (wordValue . tail . init) . splitOn ","
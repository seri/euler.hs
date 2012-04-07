main = readFile "013.in" >>= print . take 10 . show . sum . map read . lines

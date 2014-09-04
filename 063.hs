main :: IO ()
main = (print . sum . map findLimit) [1..9] where
    findLimit = floor . flip logBase 0.1 . (/10) . fromIntegral
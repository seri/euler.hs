main :: IO ()
main = (print . sum . map (floor . flip logBase 0.1 . (/10) . fromIntegral)) [1..9]
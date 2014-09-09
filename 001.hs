main :: IO ()
main = (print . sum . filter isCool) [1..999] where
    isCool n = (n `mod` 3 == 0) || (n `mod` 5 == 0)
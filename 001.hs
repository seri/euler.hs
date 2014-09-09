main :: IO ()
main = (print . sum . filter ((==0) . (`mod` 3))) [1..999]
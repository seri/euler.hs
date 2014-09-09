choose :: Int -> Int -> Integer
choose n k = (foldl f (1 :: Integer) . take k . iterate g) (n, 1) where
    f ret (p, q) = ret * (fromIntegral p :: Integer) `div` (q :: Integer)
    g (p, q) = (p - 1, q + 1)

main :: IO ()
main = print (choose 40 20)

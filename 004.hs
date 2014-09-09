isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = all (uncurry (==)) (zip xs (reverse xs))

solve :: Int
solve = maximum [ x * y | x <- range
                        , y <- range
                        , (isPalindrome . show) (x * y) ] where
    range = [100 .. 999]

main :: IO ()
main = print solve
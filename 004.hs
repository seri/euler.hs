isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = all (uncurry (==)) (zip xs (reverse xs))

range = [100 .. 999]

solve = maximum [ x * y | x <- range
                        , y <- range
                        , isPalindrome . show $ x * y ]

main = print solve

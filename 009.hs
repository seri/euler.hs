solve = head [ a*b*c | a <- [1..375]
                     , b <- enumFromTo a $ min 500 $ 750 - a
                     , let c = 1000 - a - b
                     , a^2 + b^2 == c^2 ]

main = print solve

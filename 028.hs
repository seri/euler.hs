makeSpiral n = take 4 (iterate (\x -> x - (n - 1)) (n * n))

sumSpiral n = 1 + sum (concatMap makeSpiral [3, 5..n])

main = print (sumSpiral 1001)
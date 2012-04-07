sumSqr n = div (n*(n + 1)*(2*n + 1)) 6

sqrSum n = (div (n*(n + 1)) 2) ^ 2

solve n = sqrSum n - sumSqr n

main = print $ solve 100

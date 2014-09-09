sum9 = sum [3, 3, 5, 4, 4, 3, 5, 5, 4]
sumteen = sum [6, 6, 8, 8, 7, 7, 9, 8, 8] 
sumty = sum [6, 6, 5, 5, 5, 7, 6, 6]

ten = 3
hundred = 7
andd = 3
onethousand = 11

sum10 = sum9 + ten
sum19 = sum10 + sumteen
sum99 = sum19 + sum9 * 8 + sumty * 10
sum999 = sum99 * 10 + sum9 * 100 + hundred * 900 + andd * 891
sum1000 = sum999 + onethousand

main :: IO ()
main = print sum1000
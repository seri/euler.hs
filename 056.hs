import Data.Digits

range = [1..99]

main = print $ maximum [ sum (digits 10 (a ^ b)) | a <- range, b <- range ]
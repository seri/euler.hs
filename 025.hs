fibs :: [Integer]
fibs = 1: 1: zipWith (+) fibs (tail fibs)

main :: IO ()
main = (print . (+ 1) . length . takeWhile ((< 1000) . length . show)) fibs
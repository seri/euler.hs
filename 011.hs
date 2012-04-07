import Data.Array

type Pos = (Int, Int)

dirs :: [Pos]
dirs = [(0, 1), (1, 0), (1, 1), (1, -1)]

move :: Pos -> Pos -> Pos
move (dx, dy) (x, y) = (x + dx, y + dy)

type Grid = Array Pos Int

parseInput :: String -> Grid
parseInput = listArray ((1, 1), (20, 20)) . map read . words

solve :: Grid -> Int
solve grid = maximum [ product xs | i <- indices grid
                                  , d <- dirs
                                  , let is = take 4 (iterate (move d) i)
                                  , all (inRange (bounds grid)) is
                                  , let xs = map (grid !) is ]

main = readFile "011.in" >>= print . solve . parseInput

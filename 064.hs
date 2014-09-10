sqrt' :: Int -> Int
sqrt' = floor . sqrt . fromIntegral

nonSquared :: Int -> Bool
nonSquared n = m * m < n where m = sqrt' n

-- Holds enough data for intermediate fractions
type Fractal = (Int, Int, Int)

-- (x / (sqrt n - y)) = ((sqrt n) + y) / z)
stepOne :: Fractal -> Fractal
stepOne (n, x, y) = (n, y, z) where z = (n - y * y) `div` x

-- ((sqrt + y) / z) = p + ((sqrt n) - q) / z) 
stepTwo :: Fractal -> Fractal
stepTwo (n, y, z) = (n, z, q) where
    p = floor ((sqrt (fromIntegral n) + fromIntegral y) / fromIntegral z)
    q = p * z - y

-- After step 2, p is the a(k) thing and (z / (sqrt n) - q) is the next fractal
nextFractal :: Fractal -> Fractal
nextFractal = stepTwo . stepOne

-- We keep doing these transformations until we rediscover what we started with
periodLength' :: Fractal -> Int
periodLength' frac = ((+ 1) . length . takeWhile (/= frac)) series where
    series = (tail . iterate nextFractal) frac

-- sqrt n = a(0) + sqrt n - a(0) = a(0) + (1 / 1 / (sqrt n - a(0)))
firstFractal :: Int -> Fractal
firstFractal n = (n, 1, a0) where a0 = sqrt' n

periodLength :: Int -> Int
periodLength = periodLength' . firstFractal

main :: IO ()
main = ( print . length . filter odd
       . map periodLength . filter nonSquared ) [2 .. 10000]

-- This solution is based on the "The process can be summarised as follows:"
-- part in the problem specification. It's pretty much a literal implementation
-- of that process with perhaps just a little bit of observation.
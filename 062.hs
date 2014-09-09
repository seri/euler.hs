import Data.List
import Control.Monad
import qualified Data.Map.Strict as M

type Counter = M.Map String (Int, String)

record :: String -> Counter -> Counter
record s = M.insertWith update (sort s) (1, s) where
    update (count, best) (oldCount, oldBest) = (oldCount + count, min oldBest best)

buildCounter :: Integer -> Counter
buildCounter limit = foldl insert M.empty cubeList where
    insert counter n = record (show n) counter
    cubeList = (takeWhile (<= limit) . map cube) [1..]
    cube n = n * n * n

search :: Integer -> Maybe String
search = liftM (snd . fst) . M.minView . M.filter ((== 5) . fst) . buildCounter

main :: IO ()
main = (print . search . read . flip replicate '9') 12
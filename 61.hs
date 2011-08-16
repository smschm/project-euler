import Control.Monad
import Data.List
import Common

limits = (1000,9999)
digPairs = [10..99]

figurate k n = ((k-2)*n*n + (4-k)*n) `div` 2

isInteger n = n == ((fi . floor) n)

isFigurate k n = isInteger v
    where v = (-b + sqrt (b*b - 4*a*c))/(2 * a)
          a,b,c :: Double
          a = ((fi k)/2) - 1
          b = 2 - ((fi k)/2)
          c = fi (-n)

figsTo k = takeWhile (<= (snd limits)) $
           dropWhile (<  (fst limits)) $
           map (figurate k) $ [1..]

test n = (filter (isFigurate n) [1000..9999]) == (figsTo n)

isFig k n = n `elem` (figsTo k)

mk d1 d2 = d1 * 100 + d2

cands [f1,f2,f3,f4,f5,f6] = do
    d1 <- digPairs
    d2 <- digPairs
    guard $ isFigurate f1 $ mk d1 d2
    d3 <- digPairs
    guard $ isFigurate f2 $ mk d2 d3
    d4 <- digPairs
    guard $ isFigurate f3 $ mk d3 d4
    d5 <- digPairs
    guard $ isFigurate f4 $ mk d4 d5
    d6 <- digPairs
    guard $ isFigurate f5 $ mk d5 d6
    guard $ isFigurate f6 $ mk d6 d1
    return [d1,d2,d3,d4,d5,d6]

answer = map cands $ permutations [3..8]

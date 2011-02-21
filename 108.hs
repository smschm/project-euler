import Data.Ratio

distincts n = length $ filter (isSoln n) [(n+1)..(2*n)]
isSoln n k = ((k - n) `div` (gcd (k - n) (k * n))) == 1

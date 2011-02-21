import Common
import Data.Ratio
import Debug.Trace

cand = filter (not . (`elem` (squaresTo 100000))) [1..100000]
bestApprox d n = bestApproxUnderD d (surdPfd (0,1,n)) 

answer = sum $ map denominator $ map (\x -> trace (show x) $ bestApprox (10^12) x) cand
--answer = sum $ map denominator $ map (bestApprox (10^12)) cand

main = print answer

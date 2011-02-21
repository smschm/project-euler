import Common
import Data.List

sameDigits (x,y) = sort (show x) == sort (show y)

n = [2..9999999]
cand = zip n (map eulerPhi n)
cand' = map (\(n,p) -> (n, (fi n)/(fi p))) (filter sameDigits cand)

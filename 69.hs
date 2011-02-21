import Common
import Data.List

npn n = (n, (fi n) / (fi (eulerPhi n)))

maxBySnd [] = undefined
maxBySnd l = foldl' f (head l) (tail l)

f (a,av) (b,bv) = if av > bv then (a,av) else (b,bv)

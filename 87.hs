import Common
import Data.List (nub)
import qualified Data.IntSet as S

maxn = 50000000
limit2 = ceiling (sqrt (fi maxn))
limit3 = ceiling ((fi maxn) ** (1/3))
limit4 = ceiling ((fi maxn) ** (1/4))

vsum v2 v3 v4 = v2^2 + v3^3 + v4^4

candidates = [vsum v2 v3 v4 |
	v2 <- primesTo limit2, v3 <- primesTo limit3, v4 <- primesTo limit4,
	vsum v2 v3 v4 < maxn]

import Common
import Data.List

ns = [1..100000]

rads = zip ns $ map radical ns

srads = sortBy f rads

f (a,b) (c,d) = let o = compare b d in if
	o == EQ then compare a c else o

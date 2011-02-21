import Data.List

ns = [1..12]
ofit o n = (product (map (n-) [1..o])) `div` (product [1..o])
fits o = map (ofit o) ns
(***) n v = map (n *) v
(+++) = zipWith (+)

u n = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10
us = map u ns

ufit 0 = fits 0
ufit o = let
	fitPrev = ufit (o-1)
	Just mag = find (/= 0) (us +++ ((-1) *** fitPrev))
	in fitPrev +++ (mag *** (fits o))

bop o = firstImpostor us (ufit o)
firstImpostor [] [] = undefined
firstImpostor (x:xs) (y:ys)
	| x == y = firstImpostor xs ys
	| otherwise = y

main = print $ sum $ map bop [0..9]

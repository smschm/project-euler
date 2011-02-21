import Common

rtdigits nd n = integerBisection (\x -> x*x - (n * 10^(nd*2))) 0 (n * 10^nd)

rtdigitsRound nd n = let
	rtd = rtdigits (nd+1) n
	(d,m) = rtd `divMod` 10
	in d + if m >= 5 then 1 else 0

digitsum n = sum (digitsBaseN 10 ((rtdigits 99 n) `mod` (10^99)))

cands = filter (not . (`elem` (map (^2) [2..9]))) [2..99]
answer = sum (map digitsum cands)

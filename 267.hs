import Data.Ratio

fopt = 0.202877865174037
fminus = log (1-fopt)
fplus = log (1+fopt)
stepsz = log ((1 + fopt)/(1-fopt))
mu = -0.02101523105
s = 0.4114642/2
t = log 1e9

fact n = product [2..n]
ncr n r = (fact n) `div` (fact r) `div` (fact (n-r))

digits x = round (x * 1000000000000)

val n = (sum $ map (ncr 1000) [n..1000]) % (2^1000)

flipVal x = ((head x) + fminus) : map (+ (fminus + stepsz)) x
flipProb x = map (/2) $ zipWith (+) (0:x) (x ++ [0])

flipIt x = let
	vs = map fst x
	ps = map snd x
	in zip (flipVal vs) (flipProb ps)

nubIt x = let (v,p) = last x in
	if (v > t) then (init x, p) else (x, 0)

row1 = [(0,1)] :: [(Double, Double)]

answer' 0 p x = p
answer' n p x = let
	(nx, np) = nubIt x
	nx' = flipIt nx
	in answer' (n-1) (p + np) nx'

answer = answer' 1000 0 row1

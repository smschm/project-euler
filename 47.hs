import Common

dpfs = map numDPF [0..]

answer n l p = let
	k = numDPF n
	nl = k : (take (p-1) l)
	in if (nl == (replicate p p)) then n else
		answer (n+1) nl p

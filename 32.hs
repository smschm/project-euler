import Data.List

sameDigits x y = (sort x) == (sort y)

candidates' = do
	d1 <- [1..9]
	let c2 = filter (/= d1) [1..9]
	d2 <- c2
	let c3 = filter (/= d2) c2
	d3 <- c3
	let c4 = filter (/= d3) c3
	d4 <- c4
	let c5 = filter (/= d4) c4
	d5 <- c5
	let rem = filter (/= d5) c5
	return $ (d1,d2,d3,d4,d5,rem)

candidates = candidates' >>= (\(d1,d2,d3,d4,d5,rem) ->
	[(d1, d2*1000+d3*100+d4*10+d5,rem >>= show),
	 (d1*10+d2, d3*100+d4*10+d5, rem >>= show)])

okays = filter okay candidates

okay (x,y,z) = sameDigits (show (x*y)) z

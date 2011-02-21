pents = map (\n -> (n * (3*n - 1)) `div` 2) [1..]

--isPent x = isSquare (1 + 24*x)
isInt x = x == fromIntegral (round x)
isPent x = let
	x' = fromIntegral x
	soln = (0.5 + sqrt (0.25 + 6*x'))/3
	in isInt soln

takeTo n l = takeWhile (<= n) l

answer = [(x,y) | x <- pents, y <- takeTo x pents, isPent (x+y)]

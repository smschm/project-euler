import Common

x2d x n k = digit 10 (x^2) n == k

mn p = floor$ (sqrt (1020304050607080900 + p * 100000000000000000))/100
mx p = 1 + (floor$ (sqrt (1029394959697989990 + p * 100000000000000000))/100)

candidates p = do
	x <- map (100*) [(mn p)..(mx p)]
	y <- [30,70]
	return (x + y)

d n = [(x,x^2) | x <- candidates n, (x^2) `mod` 1000 == 900,
	x2d x 4 8, x2d x 6 7, x2d x 8 6, x2d x 10 5, x2d x 12 4,
	x2d x 14 3, x2d x 16 2]

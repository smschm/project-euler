
maxs = 1000000000 `div` 3

squares = map (\x -> x*x) [1..40000]

candidates = do
    x <- [3,5..maxs]
    p <- [-1, 1]
    return (x,x+p)

radical (d,s) = (d+d+s)*(s)*(s)*(d+d-s)
-- area is a square if:
-- * (2d+s)(2d-s) is a square
-- * s^2(2d+s)(2d-s) has a factor of 16; that is, s is even
-- * if s = d + 1, then
--      (3d+1)(d-1) must be square, (3d-1)(d+1) must be square if s=d-1

--isSquare n = 

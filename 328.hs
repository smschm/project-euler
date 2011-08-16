import Data.IntMap as IM

c :: Int -> Int
c 1 = 0
c 2 = 1
c n = minimum . cs $ n
cs n = (n-1 + c (n-2)) : [g + max (c (g-1)) (g + c (n-g)) | g <- [2..n-2]]

iter l = let
	ll = length l
	v = foldl1 (zipWith (+)) [l, [1..ll], (0 : reverse l), ([1..ll-1] ++ [0])]
	in (minimum v):l

iter' l = let
	ll = length l
	in foldl1 (zipWith (+)) [l, [1..ll], (0 : reverse l), ([1..ll-1] ++ [0])]

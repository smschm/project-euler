import Data.Ratio

bd n = floor ((sqrt 0.5) * (fromIntegral n))

prob (n,b) = (b*(b-1)) % (n*(n - 1))

cands = do
	--n <- [1000000000..]
	n <- [3..]
	b <- [0,1]
	return (n,bd n + b)

answer = filter ((== 1%2) . snd) $ map (\x -> (x, prob x)) cands

import Data.List
mchain n = iterate ((`mod` n) . (* 10)) 1
fr :: (Eq a) => [a] -> Int
fr = fr' []
	where
		fr' p (l:ls) = let i = findIndex (== l) p in case i of
			Nothing -> fr' (l:p) ls
			(Just j) -> j
nc = (+1) . fr . mchain
ans = map nc [3..999]

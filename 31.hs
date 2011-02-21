values = [200,100,50,20,10,5,2]

waysToMake [] n = [replicate n 1]
waysToMake (v:vs) n
	| n == 0 = [[]]
	| n >= v = let p = (map (v:) $ waysToMake (v:vs) (n-v)) in
		p ++ waysToMake vs n
	| otherwise = waysToMake vs n

main = do
	let p = waysToMake values 200
	--print p
	print $ length p

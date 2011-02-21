import Common

pt = takeWhile (\(_,_,c) -> c <= 1000) naivePT
ss (a,b,c) = a + b + c
pm = occurrencesOf $ map ss pt

main = do
	print pt
	print pm

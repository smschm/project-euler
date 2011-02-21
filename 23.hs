import Common
import Array

-- abunds = filter isAbundant [1..28122]

main = do
	abunds <- readNumbersFromFile "abundants" :: IO [Int]
	let abArr = listArray (0,(length abunds)-1) abunds
	let sum2 = filter (not . (hasSumOfTwoInAscArray abArr)) [1..28122]
	print sum2
	print (sum sum2)

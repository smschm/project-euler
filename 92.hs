import Common
import Data.List
import System.Environment

next n = sum $ map (^ 2) $ digitsBaseN 10 n

to89 n
	| n == 1 = 0
	| n == 89 = 1
	| otherwise = seq (next n) $ to89 (next n)

sum' = foldl' (+) 0

main = do
	a <- getArgs
	print $ sum' $ map to89 $ [1..(read (head a))]

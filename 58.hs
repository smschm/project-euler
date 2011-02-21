import Common
import Text.Printf
import Debug.Trace

diagonals = scanl (+) 1 ([2,4..] >>= replicate 4)

ff True  = 1
ff False = 0

exec :: Integer -> Integer -> Integer -> [Integer] -> (Integer,Integer,Integer)
exec sl n ip diag
	| ip*10 < n = (sl,n,ip)
	| otherwise = let (dh,dt) = splitAt 4 diag in
		trace (printf "sl %d n %d primes %d\n" sl n ip)
			exec (sl+2) (n+4) (ip + sum (map (ff . isPrime) dh)) dt

diagonalTo n = takeWhile (<= n*n) diagonals

primeFrac n = let d = diagonalTo n in
	fromIntegral (length (filter isPrime d) - 1) / fromIntegral (length d)

answer = exec 3 5 3 (drop 5 diagonals)

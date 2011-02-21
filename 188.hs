import Common
import Debug.Trace
import Text.Printf

hypmod _ _ 1 = 0
hypmod a 1 n = (a `mod` n)
--hypmod a k n = (a ^ (hypmod a (k-1) (eulerPhi n))) `mod` n
hypmod a k n = trace (printf "hypmod %d %d %d" a k n) $ powmod a (hypmod a (k-1) (eulerPhi n)) n

powmod a 0 n = 1
powmod a 1 n = a `mod` n
powmod a k n = let
	resp
		| isEven k = let p2 = powmod a (k `quot` 2) n in (p2 * p2) `mod` n
		| otherwise = let p1 = powmod a (k-1) n in (p1 * a) `mod` n
	in trace (printf "powmod %d %d %d" a k n) resp

isEven = (== 0) . (`mod` 2)

import Common
import Control.Monad

m +++ n = unDigits 10 ((digitsBaseN 10 n) ++ (digitsBaseN 10 m))

checkTo4 n = do
    let p1 = n
    p2 <- primesTo p1
    guard $ isPrime (p1 +++ p2)
    guard $ isPrime (p2 +++ p1)
    p3 <- primesTo p2
    guard $ isPrime (p1 +++ p3)
    guard $ isPrime (p2 +++ p3)
    guard $ isPrime (p3 +++ p1)
    guard $ isPrime (p3 +++ p2)
    p4 <- primesTo p3
    guard $ isPrime (p1 +++ p4)
    guard $ isPrime (p2 +++ p4)
    guard $ isPrime (p3 +++ p4)
    guard $ isPrime (p4 +++ p1)
    guard $ isPrime (p4 +++ p2)
    guard $ isPrime (p4 +++ p3)
    return (p1,p2,p3,p4)

-- O(n^5) christ
checkTo5 n = do
    let p1 = n
    p2 <- primesTo p1
    --p3 <- primesTo p2
    --p4 <- primesTo p3
    --p5 <- primesTo p4
    guard $ isPrime (p2 +++ p1)
    guard $ isPrime (p1 +++ p2)
    p3 <- primesTo p2
    guard $ isPrime (p3 +++ p1)
    guard $ isPrime (p3 +++ p2)
    guard $ isPrime (p1 +++ p3)
    guard $ isPrime (p2 +++ p3)
    p4 <- primesTo p3
    guard $ isPrime (p4 +++ p1)
    guard $ isPrime (p4 +++ p2)
    guard $ isPrime (p4 +++ p3)
    guard $ isPrime (p1 +++ p4)
    guard $ isPrime (p2 +++ p4)
    guard $ isPrime (p3 +++ p4)
    p5 <- primesTo p4
    guard $ isPrime (p5 +++ p1)
    guard $ isPrime (p5 +++ p2)
    guard $ isPrime (p5 +++ p3)
    guard $ isPrime (p5 +++ p4)
    guard $ isPrime (p1 +++ p5)
    guard $ isPrime (p2 +++ p5)
    guard $ isPrime (p3 +++ p5)
    guard $ isPrime (p4 +++ p5)
    return (p1,p2,p3,p4,p5)

--answer = head $ dropWhile (/= []) $ sequence_ $ map (\x -> print x >> print (checkTo4 x)) primes

answer = answer' primes

answer' (p:ps) = do
	let v = checkTo5 p
	print p
	print v
	if v == [] then answer' ps else return ()

main = answer

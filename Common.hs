module Common where

--import qualified Codec.Encryption.RSA.NumberTheory as NT
import Array
import Data.List
import Data.Ratio
import System.IO
import qualified Data.IntMap as IM

fi a = fromInteger a

x `divides` n = (n `mod` x) == 0

primes :: [Integer]
primes = 2 : 3 : 5 : filter isPrime [7,9..]
isPrime n = and $ map (\x -> not (x `divides` n)) $ primesTo $ ceiling (sqrt (fi n))
primesTo n = takeWhile (<= n) primes

firstFactor n = let
	l = filter (\x -> x `divides` n) $ primesTo $ ceiling (sqrt (fi n))
	in case l of
		[] -> n
		h:_ -> h

firstFactorOf p n = let
	l = filter (\x -> x `divides` n) $ takeWhile (<= ceiling (sqrt (fi n))) p
	in case l of
		[] -> n
		h:_ -> h

factor = factor' primes
factor' p n
	| n <= 1 = []
	| otherwise = let f = firstFactorOf p n in
		f : factor' (dropWhile (< f) p) (n `div` f)

factorization n = map (\x -> (head x, length x)) $ group $ factor n


multiplicative :: ((Integer, Int) -> Integer) -> Integer -> Integer
multiplicative f = product . (map f) . factorization


eulerPhi = multiplicative eulerPhi'
eulerPhi' (p,k) = p^k - p^(k-1)

radical = multiplicative radical'
radical' (p,k) = p

k `powersOf` x = map (x^) [0..k]

ascDE n = ascDE' Nothing n
	where
		ascDE' _ [] = 0
		ascDE' p (l:ls) = let
			i = (if p == (Just l) then 0 else 1)
			in i + ascDE' (Just l) ls
numDPF n = ascDE $ factor n -- distinct prime factors 

--pfd :: (Frac a, Integral b) => a -> [b]
pfd x = let n = floor x in n : pfd (1/(x - (fromIntegral n)))

type Surd = (Ratio Integer, Ratio Integer, Integer)
surdPfd x@(a,b,c) = let n = floor (surdToReal x) in n : surdPfd (surdRecip (x `ssi` n))
surdCycle x@(a,b,c) = let n = floor (surdToReal x) in x : surdCycle (surdRecip (x `ssi` n))
cycleLength n = 1 + lengthToRepeat (tail (surdCycle (0,1,n)))
lengthToRepeat :: (Eq a) => [a] -> Int
lengthToRepeat = lengthToRepeat' []
lengthToRepeat' l (x:xs) = let e = x `elemIndex` l in case e of
	Nothing -> lengthToRepeat' (x:l) xs
	Just j -> j
surdToReal (a,b,c) = (fromRational a) + (fromRational b)*(sqrt (fi c))
surdRecip :: Surd -> Surd
surdRecip (a,b,c) = let rad = a*a - (fi c)*b*b
	in (a/rad, -b / rad, c)
ssi (a,b,c) n = (a - (fi n), b, c)

squaresTo n = takeWhile (<= n) (map (^2) [1..])

{- lolol
elemIndex :: (Eq e) => e -> [e] -> Maybe Int
elemIndex = elemIndex' 0
elemIndex' _ _ [] = Nothing
elemIndex' n e (x:xs) = if e == x then (Just n) else elemIndex' (n+1) e xs
-}

unPfd :: [Integer] -> Ratio Integer
unPfd (x:[]) = x%1
unPfd (x:xs) = (x%1) + 1/(unPfd xs)

increasingApprox pfd = increasingApprox' [] pfd
increasingApprox' l (p:ps) = (map ((l ++) . (:[])) [1..p]) ++
	increasingApprox' (l ++ [p]) ps
increasingApprox' _ [] = []

bestApproxUnderD d pfd = last (takeWhile ((<= d) . denominator)
	(map unPfd (increasingApprox pfd)))

hasSumOfTwoInAscArray l n = let
	hsot' (lb,ub) l n = let
		s = (l ! lb) + (l ! ub)
		v	| lb > ub = False
			| s == n = True
			| s > n = hsot' (lb,ub-1) l n
			| s < n = hsot' (lb+1,ub) l n
		in v
	in hsot' (bounds l) l n

hGetLines h = do
	iseof <- hIsEOF h
	if iseof then return [] else do
		l <- hGetLine h
		restLines <- hGetLines h
		return (l:restLines)

readNumbersFromFile fname = do
	h <- openFile fname ReadMode
	numstrs <- hGetLines h
	return $ map read numstrs

digitsBaseN b n = unfoldr (\x -> if x == 0 then Nothing else
	let (q,r) = x `divMod` b in Just (r,q)) n
digitsBaseN' b n = unfoldr (\x -> let (q,r) = x `divMod` b in Just (r,q)) n

digit b n k = (digitsBaseN' b n) !! k

unDigits b [] = 0
unDigits b (x:xs) = x + b * unDigits b xs

pytriple n m = (n*n-m*m,2*m*n,n*n+m*m)
pytriples n = map (pytriple n) [1..n-1]
pytriplesto n = concat $ map pytriples [2..n]

occurrencesOf l = IM.toAscList $ foldr (IM.alter count) IM.empty l
	where
		count Nothing = Just 1
		count (Just n) = Just (n+1)
{-
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = undefined
foldl1' f l = foldl' f (head l) (tail l)
-}

maxBySnd, minBySnd :: (Ord a) => [(b,a)] -> (b,a)
maxBySnd = foldl1' (findMost (>))
minBySnd = foldl1' (findMost (<))

findMost :: (a -> a -> Bool) -> (b, a) -> (b, a) -> (b, a)
findMost f (a,av) (b,bv) = if f av bv then (a,av) else (b,bv)

naivePT :: [(Int,Int,Int)]
naivePT = [(a,b,c) | c <- [1..], a <- [1..c], b <- [1..a], a*a + b*b == c*c]

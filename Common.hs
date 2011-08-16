module Common where

--import qualified Codec.Encryption.RSA.NumberTheory as NT
import Array
import Data.List
import Data.Ratio
import System.IO
import qualified Data.IntMap as IM
import qualified Data.Map as M

fi a = fromInteger a

x `divides` n = (n `mod` x) == 0

primes :: [Integer]
primes = 2 : 3 : 5 : filter isPrime [7,9..]
isPrime n = and $ map (\x -> not (x `divides` n)) $ primesTo $ ceiling (sqrt (fi n))
primesTo n = takeWhile (<= n) primes

-- first prime factor of n
firstFactor n = let
	l = filter (\x -> x `divides` n) $ primesTo $ ceiling (sqrt (fi n))
	in case l of
		[] -> n
		h:_ -> h

-- first prime factor of n (that's in some ascending list of primes p,
-- "firstFactor" should be equivalent to "firstFactorOf primes"
firstFactorOf p n = let
	l = filter (\x -> x `divides` n) $ takeWhile (<= ceiling (sqrt (fi n))) p
	in case l of
		[] -> n
		h:_ -> h

isSquare :: Integer -> Bool
isSquare 1 = True
isSquare n = let ff = firstFactor n
                 (q,r) = n `divMod` ff
                 (q',r') = q `divMod` ff
             in if r' == 0 then isSquare q' else False

-- factorization of a number, in list of primes form
factor = factor' primes
factor' p n
	| n <= 1 = []
	| otherwise = let f = firstFactorOf p n in
		f : factor' (dropWhile (< f) p) (n `div` f)

-- factorization, in list of (prime, exponent)
factorization n = map (\x -> (head x, length x)) $ group $ factor n

-- make a multiplicative function for any integer in terms of a function
-- that takes a (prime, exponent) argument
multiplicative :: ((Integer, Int) -> Integer) -> Integer -> Integer
multiplicative f = product . (map f) . factorization

-- euler phi function. durp
eulerPhi = multiplicative eulerPhi'
eulerPhi' (p,k) = p^k - p^(k-1)

-- radical of an integer
radical = multiplicative radical'
radical' (p,k) = p

eulerSigma = multiplicative esig'
esig' (p,k) = (p^(k+1) - 1) `div` (p - 1)

squarefree = multiplicative sf'
    where sf' (p,k) = if k > 1 then 0 else 1

-- 1, x, x^2, ..., x^k
k `powersOf` x = map (x^) [0..k]

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- number of Distinct Elements in a list
-- I think I initially intended this to work only on ascending
-- lists for some reason but that's not really necessary,
-- only that an element won't appear in a list outside of a
-- single group of that element
ascDE n = ascDE' Nothing n
	where
		ascDE' _ [] = 0
		ascDE' p (l:ls) = let
			i = (if p == (Just l) then 0 else 1)
			in i + ascDE' (Just l) ls
numDPF n = ascDE $ factor n -- distinct prime factors 

-- Partial Fraction Decomposition
-- will be wrong after a few terms for doubles and run into
-- an exception on ratios, so watch your shit son
--pfd :: (Frac a, Integral b) => a -> [b]
pfd x = let n = floor x in n : pfd (1/(x - (fromIntegral n)))

-- I cannot for the life of me remember how this works, but:
-- a Surd (a,b,c) represents a+b*sqrt(c), and surdPfd gives you
-- the PFD of that number without losing precision like pfd above
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

-- convert a PFD back into a ratio
unPfd :: [Integer] -> Ratio Integer
unPfd (x:[]) = x%1
unPfd (x:xs) = (x%1) + 1/(unPfd xs)

-- given a PFD, returns monotonically 'better' approximations to
-- the number the PFD represents, with increasing denominator
increasingApprox pfd = increasingApprox' [] pfd
increasingApprox' l (p:ps) = (map ((l ++) . (:[])) [1..p]) ++
	increasingApprox' (l ++ [p]) ps
increasingApprox' _ [] = []

-- best rational approximation to a number with denominator <= d
bestApproxUnderD d pfd = last (takeWhile ((<= d) . denominator)
	(map unPfd (increasingApprox pfd)))

-- are there two elements in an ascending array that sum to n?
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

-- digits of n in base b, little endian
digitsBaseN b n = unfoldr (\x -> if x == 0 then Nothing else
	let (q,r) = x `divMod` b in Just (r,q)) n
-- same as above, but NEVER STOP
digitsBaseN' b n = unfoldr (\x -> let (q,r) = x `divMod` b in Just (r,q)) n

-- b^k digit base-b of n
digit b n k = (digitsBaseN' b n) !! k

-- convert digit-list back to number
unDigits b [] = 0
unDigits b (x:xs) = x + b * unDigits b xs

-- this does not work
pytriple n m = (n*n-m*m,2*m*n,n*n+m*m)
pytriples n = map (pytriple n) [1..n-1]
pytriplesto n = concat $ map pytriples [2..n]

-- from a list of Ints, gives an (int, count) list of how many times
-- each appears in the list
-- hopefully, somewhat efficiently
occurrencesOf l = IM.toAscList $ foldr (IM.alter count) IM.empty l
	where
		count Nothing = Just 1
		count (Just n) = Just (n+1)

occurrencesOfM l = M.toAscList $ foldr (M.alter count) M.empty l
	where
		count Nothing = Just 1
		count (Just n) = Just (n+1)
{-
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ [] = undefined
foldl1' f l = foldl' f (head l) (tail l)
-}

-- max/min of a list of (x,y) by the second part
maxBySnd, minBySnd :: (Ord a) => [(b,a)] -> (b,a)
maxBySnd = foldl1' (findMost (>))
minBySnd = foldl1' (findMost (<))

ascBySnd = ascBySnd' 0 (>)
ascBySnd' _ f [] = []
ascBySnd' b f ((a,v):vs) = if f v b then (a,v) : ascBySnd' v f vs else ascBySnd' b f vs

findMost :: (a -> a -> Bool) -> (b, a) -> (b, a) -> (b, a)
findMost f (a,av) (b,bv) = if f av bv then (a,av) else (b,bv)

-- every pythagorean triple, comes very slowly so you can savor them all
naivePT :: [(Int,Int,Int)]
naivePT = [(a,b,c) | c <- [1..], a <- [1..c], b <- [1..a], a*a + b*b == c*c]

partList = map partitions [0..]

a !!! b
    | b <= 0 = 0
    | otherwise = a !! b

partitions :: Int -> Integer
partitions n
    | n <= 0   = 0
    | n == 1   = 1
    |otherwise = sum $ map (p' n) [1..n]
        where p' n k = (-1)^(k+1) *
                       (partList !!! (n - k*(3*k-1)`div`2) +
                        partList !!! (n - k*(3*k+1)`div`2))

factorial n = product [2..n]
choose n k = (factorial n) `div` (factorial k) `div` (factorial (n-k))


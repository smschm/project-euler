import Common
import Data.List

isPandigital n = sort n == [1..9]

fibs9 :: [Int]
fibs9 = 1 : 1 : zipWith (\x y -> (x + y) `mod` 1000000000) fibs9 (tail fibs9)


---------- uuuuuuuuuuuuuuuuuggggggggggggggggggghhhhhhhhhhhh
---------- feel free to shit your pants
highFibs' :: [(Double, Int)]
highFibs' = (1,0) : (1,0) : zipWith hf' highFibs' (tail highFibs')
    where hf' (x,e) (y,e') = let v = case (e' - e) of
                                         0 -> x + y 
                                         1 -> y + x/10
                             in  if v > 1000000000 then (v/10,e'+1) else (v,e')
highFibs = map (floor . fst) highFibs'

satisfiesL n = sort (digitsBaseN 10 n) == [1..9]

highDigits :: Integer -> Integer
highDigits n = n `div` (10 ^ (floor ((log (fi n))/(log 10)) - 8))

satisfies n = (satisfiesL n) -- && (satisfiesL (highDigits n))
    where d = digitsBaseN 10 n

--satisfiesR n = isPandigital (digitsBaseN 10 (highDigits (fibs !! (n-1))))

{-
satisfiesR :: Int -> Bool
satisfiesR n = sort (take 9 (reverse d)) == [1..9]
    where d = digitsBaseN 10 (fibs !! (n-1))
-}

satisfiesR :: Int -> Bool
satisfiesR n = sort d == [1..9]
    where d = digitsBaseN 10 (highFibs !! (n-1))



candsL = map fst $ filter (satisfiesL . snd) $ zip [1..] fibs9
candsR = filter (satisfiesR) [1..]

answer = filter (satisfiesR . fi) candsL

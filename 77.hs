import Common

waysToWrite :: [Integer] -> Integer -> [[(Integer,Integer)]]
waysToWrite p n
    | n == 0    = [[]]
    | n == 1    = []
    | otherwise = do
        tp <- takeWhile (<= n) p
        v <- [1..(n `div` tp)]
        other <- waysToWrite (dropWhile (<= tp) p) (n - tp * v)
        return $ (tp,v) : other

nw = length . (waysToWrite primes)

answer = map (\x -> (x, nw x)) [1..]

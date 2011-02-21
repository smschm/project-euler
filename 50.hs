import Common

primeSums = primeSums' primes
primeSums' p n = (sum $ take n p) : primeSums' (tail p) n

psto1m = (takeWhile (<= 1000000)) . primeSums

candidates = [600,599..]
answer = dropWhile ((== []) . snd) $ map (\x -> (x, filter isPrime $ psto1m x)) candidates

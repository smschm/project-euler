-- brute force
--

cands n = [((n)`div`3 + 1).. (n-1)`div`2]
reduced n k = (gcd n k) == 1
reduceds n = filter (reduced n) (cands n)
nReds n = length $ reduceds n

x = map nReds [5..12000]
answer = sum x

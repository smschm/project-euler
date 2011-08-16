import Common

digitsAsc = da' (-1) 
    where da' d [] = True
          da' d (x:xs)
            | x >= d = da' x xs
            | otherwise = False

digitsDesc = dd' 10
    where dd' d [] = True
          dd' d (x:xs)
            | x <= d = dd' x xs
            | otherwise = False

bouncy n = not (digitsAsc d || digitsDesc d)
    where d = digitsBaseN 10 n

answer = answer' 0 0
answer' b n = let n' = n + 1
                  b' = b + if bouncy n' then 1 else 0
              in  if (b' * 100) == (n' * 99) then n' else answer' b' n'

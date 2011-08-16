import Data.List
import Control.Monad

type Hit = (Int, Int)

value (x,y) = x * y

doubles = map (\x -> (x,2)) $ [1..20] ++ [25]
allscores = (0,0) : doubles ++ (map (\x -> (x,1)) $ [1..20] ++ [25]) ++
                               (map (\x -> (x,3)) $ [1..20])


nubf (a,b,c) (d,e,f) = (a == e) && (b == d)

checkouts' n = do
    final <- doubles
    guard $ n - (value final) >= 0
    second <- allscores
    guard $ n - (value second) >= 0
    first <- allscores
    guard $ n - (value first) >= 0
    --let first = n - (value final) - (value second)
   -- guard $ (value first) >= 0
   -- guard $ first `elem` allscores
    guard $ (value final + value first + value second) == n
    return (first,second,final)

checkouts n = nubBy nubf $ checkouts' n

ways = map (length . checkouts) [1..99]

answer = sum ways

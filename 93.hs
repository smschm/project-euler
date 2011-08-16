import Control.Monad.State
import Data.List
import Common

data Op = Add | Sub | Mul | Div deriving (Show, Eq)

data BT = Branch Op BT BT | Leaf Int deriving (Show, Eq)

{-
    (_*_)*(_*_) -> * (* _ _) (* _ _)
    ((_*_)*_)*_ -> * (* (* _ _) _) _
    (_*(_*_))*_ -> * (* _ (* _ _)) _
    _*((_*_)*_) -> * _ (* (* _ _) _)
    _*(_*(_*_)) -> * _ (* _ (* _ _))
-}
bts = [(\o1 o2 o3 n1 n2 n3 n4 ->
    Branch o1 (Branch o2 (Leaf n1) (Leaf n2))
              (Branch o3 (Leaf n3) (Leaf n4))),
       (\o1 o2 o3 n1 n2 n3 n4 ->
    Branch o1 (Branch o2 (Branch o3
        (Leaf n1) (Leaf n2)) (Leaf n3)) (Leaf n4)),
       (\o1 o2 o3 n1 n2 n3 n4 ->
    Branch o1 (Branch o2 (Leaf n1) (Branch o3
        (Leaf n2) (Leaf n3))) (Leaf n4)),
       (\o1 o2 o3 n1 n2 n3 n4 ->
    Branch o1 (Leaf n1) (Branch o2 (Branch o3
        (Leaf n2) (Leaf n3)) (Leaf n4))),
       (\o1 o2 o3 n1 n2 n3 n4 ->
    Branch o1 (Leaf n1) (Branch o2 (Leaf n2)
        (Branch o3 (Leaf n3) (Leaf n4))))]

eval (Leaf n) = [n]
eval (Branch op t1 t2) = case op of
    Add -> liftM2 (+) (eval t1) (eval t2)
    Sub -> liftM2 (-) (eval t1) (eval t2)
    Mul -> liftM2 (*) (eval t1) (eval t2)
    Div -> do
        e1 <- eval t1
        e2 <- eval t2
        if (e2 == 0) then fail "" else
            let (q,r) = e1 `divMod` e2 in
                if (r == 0) then return q else fail ""

ops = [Add, Sub, Mul, Div]

trees digits = do
    t <- bts
    o1 <- ops
    o2 <- ops
    o3 <- ops
    dig <- permutations digits
    let (n1:n2:n3:n4:_) = dig
    return $ t o1 o2 o3 n1 n2 n3 n4

candidates = do
    a <- [1..9]
    b <- [a+1..9]
    c <- [b+1..9]
    d <- [c+1..9]
    return [a,b,c,d]

firstGap oc = fg' (dropWhile (<= 0) (map fst oc))
fg' [x] = x+1
fg' (x:xs:xss) = if xs == x + 1 then fg' (xs:xss) else x

poss d = firstGap $ occurrencesOf $ (trees d >>= eval)

answer = map (\x -> (x, poss x)) candidates

--the29s = filter (\t -> eval t == [29]) trees



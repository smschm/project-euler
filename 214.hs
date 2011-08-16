import Common

--chainLength :: Integer -> IO Integer
chainLength 1 = 1
chainLength n = 1 + chainLength (eulerPhi n)

pt = primesTo (4*10^7)

cands = filter (\x -> chainLength x == 25) pt

answer = sum cands

test = ascBySnd $ map (\x -> (x, chainLength x)) pt

main = do
    print test
    print cands
    print answer

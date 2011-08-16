import Data.List
import System.IO.Unsafe
import Common

factorial n = product [2..n]

facSum :: Integer -> Integer
facSum n = sum $ map factorial (digitsBaseN 10 n)

chainSize f l ls n = let n' = f n
                         p  = elem n' l
                     in  case p of
                            False -> chainSize f (n' : l) (ls + 1) n'
                            True -> ls

facChainSize = chainSize facSum [] 0

answers = filter (\x -> seq (debug x) $ facChainSize x == 59) [1..1000000]

debug x = unsafePerformIO $
    if x `mod` 100000 == 0 then (print x) else return ()

main = do
    print answers
    print (length answers)

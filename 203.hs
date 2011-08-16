import Common
import Data.List

isSquarefree = (==1) . squarefree
pt = ptrow =<< [0..50]
ptrow n = map (choose n) [0..n]
ptuniq = nub pt
answer = sum $ filter isSquarefree $ ptuniq

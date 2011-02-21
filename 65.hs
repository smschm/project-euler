import Common
import Data.Ratio

epfd = 2 : ([1..] >>= e')
e' n = [1,2*n,1]

unpfd :: [Integer] -> Ratio Integer
unpfd (l:[]) = l % 1
unpfd (l:ls) = (l % 1) + 1/(unpfd ls)

econv n = unpfd $ take n epfd

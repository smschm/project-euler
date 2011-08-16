import Common
import Data.Ratio
import Data.List

square x = x * x

squares = map square [1..31]

candidates = filter (`notElem` squares) [2..1000]

isPell n r = (==1) $ (square (numerator r)) - n * (square (denominator r))

unPfd' :: [Integer] -> [Ratio Integer]
unPfd' p = map (\x -> unPfd (take x p)) [1..]

convergents n = unPfd' $ surdPfd (0,1%1,n)
fundamental n = head $ filter (isPell n) $ convergents n

max' = 16421658242965910275055840472270471049

answer = filter ((== max') . numerator . fundamental) candidates

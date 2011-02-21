import Data.List
import System.IO.Unsafe
import Common

l [] = [[]]
l (1:0:xs) = let s = l xs in map ((0:) . (2:)) s ++ map ((1:) . (0:)) s
l (1:xs) = map (1:) (l xs)
l (0:xs) = map (0:) (l xs)
l (2:0:xs) = let s = l xs in map ((1:) . (2:)) s ++ map ((2:) . (0:)) s
l (2:xs) = map (2:) (l xs)
l x = unsafePerformIO $ (print x) >> return [[]]

next p = nub $ p >>= l

initi = reverse $ digitsBaseN 2 (10^25)

main = print $ length $ [initi] >>= l >>= l --next [initi]

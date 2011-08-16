import Common
import qualified Data.IntMap as IM
import Data.List

limit = 10000

loop m f = loop' m f []
loop' :: (IM.IntMap Int) -> (Int -> Int) -> [Int] -> Int -> Int
loop' m f l n = let n' = f n
                    pos = elemIndex n' l
                    v = if n' > limit then ([],0)
                        else case pos of
                            Nothing -> loop' m f (n':l) n'
                            Just j  -> j
                in  IM.findWithDefault v n m

divsum x = (eulerSigma x) - x

main = print $ maxBySnd $ map (loop IM.empty divsum) [1..limit]

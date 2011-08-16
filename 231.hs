import Common
import qualified Data.IntMap as IM

type IMN = IM.IntMap Int

im :: Integer -> IMN
im n = IM.fromAscList $ map (\(x,y) -> (fi x,fi y)) $
               factorization n

imMul :: IMN -> IMN -> IMN
imMul = IM.unionWith (+)

--imDiv x y = IM.unionWith (+) x (map (\v -> 0 - v) y)

-- this pleases me. i am pleased

import Common
import System.IO.Unsafe
import qualified Data.Map as M

erasures dl d = sequence $
                map (\d' -> if d == d' then [Nothing, Just d'] else [Just d']) dl

erasure n = filter hasErasure $ (erasures (digitsBaseN 10 n)) =<< [0..9]

hasErasure [] = False
hasErasure (Nothing:_) = True
hasErasure ((Just _):xs) = hasErasure xs

answer = find8 M.empty primes

incM k = M.alter inc' k
         where inc' Nothing = Just 1
               inc' (Just j) = Just (j+1)

find8 m (p:ps) = let es = erasure p
                     (m',ans) = foldIn es m
                 in  case ans of
                     Nothing -> find8 m' ps
                     Just j  -> j

foldIn [] m = (m, Nothing)
foldIn (k:ks) m = let m' = incM k m
                      c = m' M.! k
                  in  case c of
                      8 -> (m', Just k)
                      _ -> foldIn ks m'

main = print answer

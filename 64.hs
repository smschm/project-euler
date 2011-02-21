import Common

cand = filter (not . (`elem` (map (^2) [1..99]))) [2..9999]

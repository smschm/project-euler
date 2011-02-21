import Common

deviation n = (n, abs $ (fi (round ((fi n) * (3 / 7))))/(fi n) - (3 / 7))

ansTo n = minBySnd $ map deviation $ filter (\x -> x `mod` 7 /= 0) [1..n]

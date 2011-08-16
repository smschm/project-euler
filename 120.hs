val a n = ((a-1)^n + (a+1)^n) `mod` (a^2)

maxr n = maximum . map (val n) $ [0..n]
maxr' n = maximum . map (val n) $ [0..(3*n)]

ans = map maxr' $ [3..1000]

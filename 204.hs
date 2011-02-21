import Common
--import Debug.Trace

trace _ x = x

ham [k] n = (floor ((log (fi n))/(log (fi k)))) + 1
ham (k:k':ks) n
	| n == 1 = trace "** ham 1" 1
	| n == 0 = trace "** ham 0" 0
	| otherwise = trace ("ham " ++ show k ++ " " ++ show n) $ sum $ map (ham (k':ks)) $ powerdown n k

powerdown n k = takeWhile (> 0) $ map floor $
	iterate (/ (fi k)) (fi n)

answer = ham (reverse (primesTo 100)) 1000000000

main = print answer

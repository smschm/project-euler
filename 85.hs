
rects x y = sum $ do
	x' <- [1..x]
	y' <- [1..y]
	return ((x + 1 - x')*(y + 1 - y'))

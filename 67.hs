import Text.ParserCombinators.Parsec

number = do
	x <- many1 digit
	return ((read x) :: Int)

spaces' = many $ char ' '
newline' = char '\r' >> char '\n'

numberline = many $ do
	x <- number
	spaces'
	return x

numbers = many1 $ do
	x <- numberline
	newline'
	return x

solve (x:[]) = x
solve (x:xs) = zipWith3 g x y (tail y)
	where
		g a b c = a + max b c
		y = solve xs

getNumbers = do
	x <- readFile "triangle.txt"
	let (Right r) = parse numbers "" x
	return r

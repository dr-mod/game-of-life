import Control.Concurrent

outerFrame :: (Int, Int) -> [(Int, Int)]
outerFrame (x, y) = [(f, s) | f <- [x-1..x+1], s <- [y-1..y+1], not (x == f && s == y)]

rmOut :: [[Int]] -> [(Int, Int)] -> [(Int, Int)] 
rmOut _ [] = []
rmOut board ((x,y):xys) = if y < 0 || y >= length board 
								|| x < 0 || x >= length (board!!y)
								then rmOut board xys 
								else (x,y) : rmOut board xys

cellStatus :: Int -> Int -> Int
cellStatus number alive | number == 3 || number == 2 && alive == 1 = 1 | otherwise = 0

getValue :: [[Int]] -> (Int, Int) -> Int
getValue board (x, y) = board !! y !! x

nextStatus :: [[Int]] -> (Int, Int) -> Int
nextStatus board point = cellStatus (sum (map (getValue board) (rmOut board (outerFrame point)))) (getValue board point)

goThrough :: [[Int]] -> ((Int, Int) -> Int) -> [[Int]]
goThrough board nextStatus = [[nextStatus (x, y) | x <- [0..(length (board!!0))-1]] | y <- [0..(length board)-1]]

outTrans 0 = ' '
outTrans 1 = '*'
inTrans '*' = 1
inTrans _ = 0

clean = do putStr "\ESC[2J\ESC[H"

run board = do
	let view = map (\x -> map outTrans x) board
	putStr "\ESC[H"
	mapM_ putStrLn view
	let nextGen = goThrough board (nextStatus board)
	threadDelay 50000 
	run nextGen
main = do
	clean
	content <- readFile ("init.txt")
	run . map (\x -> map inTrans x) $ lines content
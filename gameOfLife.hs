import Control.Concurrent

outerFrame :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
outerFrame (x, y) (width, height) = [(f, s) | 
							f <- [x-1..x+1], 
							s <- [y-1..y+1], 
							not (x == f && s == y), 
							not (f < 0 || f >= width),
							not (s < 0 || s >= height)]

cellStatus :: Int -> Int -> Int
cellStatus number alive | number == 3 || number == 2 && alive == 1 = 1 
						| otherwise = 0

getValue :: [[Int]] -> (Int, Int) -> Int
getValue board (x, y) = board !! y !! x

nextPointStatus :: [[Int]] -> (Int, Int) -> Int
nextPointStatus board point = let 
								getBoardValue = getValue board
								currentValue = getBoardValue point
								boardSize = (length (board!!0), length board)
							  in cellStatus (sum . map getBoardValue $ outerFrame point boardSize) currentValue

goThrough :: [[Int]] -> [[Int]]
goThrough board = [[nextStatus (x, y) | x <- [0..(length (board!!0))-1]] | y <- [0..(length board)-1]]
				  where nextStatus = nextPointStatus board

outTrans 0 = ' '
outTrans 1 = '*'
inTrans '*' = 1
inTrans _ = 0

clean = do putStr "\ESC[2J\ESC[H"

run board = do
	let view = map (\x -> map outTrans x) board
	putStr "\ESC[H"
	mapM_ putStrLn view
	let nextGen = goThrough board
	threadDelay 50000 
	run nextGen
main = do
	clean
	content <- readFile ("init.txt")
	run . map (\x -> map inTrans x) $ lines content
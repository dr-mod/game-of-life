import Control.Concurrent

outerFrame :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
outerFrame (width, height) (x, y) = [(f, s) | f <- [x-1..x+1], s <- [y-1..y+1], 
                                    not (x == f && s == y), 
                                    not (f < 0 || f >= width || s < 0 || s >= height)]

cellStatus :: Int -> Int -> Int
cellStatus number alive | number == 3 || number == 2 && alive == 1 = 1 
                        | otherwise = 0
                        
fstLayer :: [[Int]] -> [[Int]] -> Int -> [[Int]]
fstLayer _ [] _ = []
fstLayer board (xb:xbs) number = sndLayer board xb (0, number) : fstLayer board xbs (number + 1)

sndLayer :: [[Int]] -> [Int] -> (Int, Int) -> [Int]
sndLayer _ [] _ = []
sndLayer board (xb:xbs) point = let 
                                    getBoardValue (x, y) = board !! y !! x
                                    frame = outerFrame (length (board!!0), length board)
                                in cellStatus (sum . map getBoardValue $ frame point) xb : sndLayer board xbs (fst point + 1, snd point)

outTrans 0 = ' '
outTrans 1 = '*'
inTrans '*' = 1
inTrans _ = 0

clean = do putStr "\ESC[2J\ESC[H"

run board = do
    let view = map (\x -> map outTrans x) board
    putStr "\ESC[H"
    mapM_ putStrLn view
    let nextGen = fstLayer board board 0
    threadDelay 50000 
    run nextGen
main = do
    clean
    content <- readFile ("init.txt")
    run . map (\x -> map inTrans x) $ lines content
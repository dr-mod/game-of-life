import Control.Concurrent

colGrn = "\ESC[0;32m"
colNo = "\ESC[0m"

curTop = "\ESC[H"
curClear = "\ESC[2J" ++ curTop

outerFrame :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
outerFrame (w, h) (x, y) = [(bar w f, bar h s) | f <- [x-1..x+1], s <- [y-1..y+1], 
                            not (x == f && s == y)]
                            where 
                                bar bound p 
                                    | p < 0 = bound - 1
                                    | p >= bound = 0
                                    | otherwise = p

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

color :: [Char] -> [Char] -> [Char]                                
color [] _ = []
color (c:cs) (n:ns) 
    | n == '*' && c /= n = colGrn ++ n : colNo ++ other
    | otherwise = n : other
    where
        other = color cs ns

outTrans 0 = ' '
outTrans 1 = '*'
inTrans '*' = 1
inTrans _ = 0

viewToStr :: [[Int]] -> [Char]
viewToStr board = foldl (\acc x -> acc ++ x ++ "\n") "" $ map (map outTrans) board

run board = do
    let nextGen = fstLayer board board 0
    let colorized = color (viewToStr board) (viewToStr nextGen)
    putStr (curTop ++ colorized)
    threadDelay 50000 
    run nextGen
main = do
    putStr curClear
    content <- readFile ("init.txt")
    run . map (\x -> map inTrans x) $ lines content
dostuff::[[a]]->[(Int,Int)]
dostuff grid = 
    do
        (y, row) <- zip [0..] grid
        (x, _) <- zip [0..] row
        return (x,y)

dostuff'::[[a]]->[(Int,Int)]
dostuff' grid = [(x,y) | (y,row) <- zip [0..] grid, (x, _) <- zip [0..] row]

prop_dostuff :: [[a]]->Bool
prop_dostuff x = dostuff x == dostuff' x

dostuff2 :: [[a]] -> [[(Int, Int)]]
dostuff2 grid = 
    do
        (y, row) <- zip [0..] grid
        return (
            do
                (x, _) <- zip [0..] row
                return (x,y))

dostuff2' :: [[a]] -> [[(Int, Int)]]
dostuff2' grid = [[(x,y) |  (x,_) <- zip [0..] row] | (y, row) <- zip [0..] grid]

prop_dostuff2 :: [[a]]->Bool
prop_dostuff2 x = dostuff2 x == dostuff'2 x
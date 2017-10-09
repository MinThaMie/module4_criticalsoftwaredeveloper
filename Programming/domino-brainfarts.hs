verticalTestGrid :: Grid -> Grid
verticalTestGrid g = transpose g

horizontalOptions :: Grid -> [(Int, Int)]
horizontalOptions g = concat (map tuples g)

verticalOptions :: Grid -> [(Int, Int)]
verticalOptions g = concat (map tuples (verticalTestGrid g))

allOptions :: Grid -> [(Int, Int)]
allOptions g = (verticalOptions g) ++ (horizontalOptions g)

tuples :: [Int] -> [(Int, Int)] -- Combinations as they appear on the board
tuples []       = []
tuples [x]      = []
tuples (x:y:xs) =  (x, y) : tuples (y:xs) 

tuplesOrd :: [Int] -> [(Int, Int)] -- Combinations where they are ordered to match a Bone
tuplesOrd []       = []
tuplesOrd [x]      = []
tuplesOrd (x:y:xs) | x < y = (x, y) : tuplesOrd (y:xs)
                   | otherwise = (y, x) : tuplesOrd (y:xs) 
 

uniques :: [(Int,Int)] -> [(Int,Int)] -- Does not work on allOptions now
uniques [] = []
uniques (x:xs) | x `elem` xs = uniques (filter (/= x) xs)
               | otherwise   = x : uniques xs




neighbours :: Int -> Grid -> [Square]
neighbours n g = concat (map maybeToList ([upneigh n g] ++ [downneigh n g] ++ [leftneigh n g] ++ [rightneigh n g]))

upneigh :: Int -> Grid -> Maybe Square
upneigh n g = if (n - 8) > 0              then Just (g !! (n - 8)) else Nothing

downneigh n g = if (n + 8) < (length g)-1 then Just (g !! (n + 8)) else Nothing

leftneigh n g = if n `mod` 8 /= 0         then Just (g !! (n - 1)) else Nothing

rightneigh n g = if n `mod` 8 /= 7        then Just (g !! (n + 1)) else Nothing
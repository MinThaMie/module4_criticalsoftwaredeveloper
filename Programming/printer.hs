field :: [Int]
field = [0..55]

printField = putStr . unlines $ map show (chop 8 field)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)
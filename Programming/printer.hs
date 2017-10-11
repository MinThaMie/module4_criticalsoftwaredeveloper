field :: [Int]
field = [0..55]

printField = putStr . unlines $ map showRow (chop 8 field)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

showRow :: [Int] -> String
showRow = concat . map showNum

showNum :: Int -> String
showNum n | n < 10    = "  " ++ show n ++ " "
          | otherwise = " " ++ show n ++ " "
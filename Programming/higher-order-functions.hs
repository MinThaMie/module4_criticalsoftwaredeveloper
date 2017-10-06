-- Exercise 1
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]
map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs
-- Exercise 2
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]
filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f [] = []
filter'' f (x:xs)   | f x = [x] ++ filter'' f xs
                    | otherwise = filter'' f xs
-- Exercise 3  [f x | x <- xs, p x]
mapNfilter f p xs = map f (filter p xs)

-- Exercise 4
all' :: (a -> Bool) -> [a] -> Bool
all' f [] = False
all' f (x:xs)   | f x && all' f xs = True
                | otherwise = False

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs)   | f x = True
                | otherwise = any' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = [x] ++ takeWhile' f xs
                    | otherwise = []
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = x:xs
-- Exercise 5
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr(\x xs -> f x : xs) []

filterFoldr :: (a -> Bool) -> [a] -> [a] 
filterFoldr p = foldr(\x xs -> if p x then x : xs else xs) []
-- Exercise 6
dec2int :: [Int] -> Int
dec2int = foldl(\x xs -> x * 10 + xs) 0

-- Exercise 7
compose :: [a -> a] -> (a -> a) 
compose = foldr (.) id
-- sumSqrEven = compose [sum, map (^2), filter even] is wrong because the types of sum, map and filter are different
sumSqrEven = sum . map (^2) . filter even

--Exercise 8
curry' :: ((a , b) -> c) -> a -> b -> c
curry' f = (\x -> (\y -> f (x, y)))
uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f = \(x, y) -> f x y

-- -- Exercise 9
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

-- map f, and iterate f using unfold
mapUnfold f = unfold null (f . head) (tail)

iterateUnfold f = unfold (const False) id f

-- Exercise 12
altMap :: (a -> b) -> (a ->b) -> [a] -> [b]
altMap f g [] = []
altMap f g [x] = f x : []
altMap f g (x:y:xs) = f x : g y : altMap f g xs

-- Exercise 13
luhn :: [Int] -> Bool -- might go wrong with uneven numbers, see wiki
luhn xs = (sum (altMap nothing luhnDouble xs)) `mod` 10 == 0
            where
                nothing = (+0)

luhnDouble :: Int -> Int
luhnDouble x    | (x * 2) > 9 = (x*2) - 9
                | otherwise = x * 2




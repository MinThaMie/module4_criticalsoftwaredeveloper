sum'' :: Num a => [a] -> a
sum'' [] = 0
sum'' (n:ns) = n + sum'' ns 

mult :: Int -> Int -> Int
mult n 0 = 0
mult n m = n + (mult n (m-1))

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib(n-1) + fib (n-2)

even' :: Int-> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)

evens :: [a] -> [a] -- on even indices
evens [] = []
evens [x] = [x]
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [x] = []
odds (_:xs) = evens xs

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x : y : ys
                | otherwise = y : insert x ys

isort :: Ord a => [a] -> [a] -- Inefficient for large lists
isort [] = []
isort (x:xs) = insert x (isort xs)

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort  (x:xs) =  sort smaller ++ [x] ++ sort larger -- chooses sort based on length of list
where
  smaller = [n | n <- xs, n <= x]
  larger = [n | n <- xs,  n > x]

sort :: Ord a => [a] -> [a]
sort xs | length xs < 10    = isort xs
        | otherwise         = qsort xs


sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

exponentiation :: Int -> Int -> Int
exponentiation x 1 = x
exponentiation x y = (exponentiation x 1) * (exponentiation x (y-1))

euclid :: Int -> Int -> Int
euclid x y  = if x == y then x
            else  euclid smallest div
            where 
                smallest = minimum[x,y]
                largest = maximum[x,y]
                div = largest - smallest
-- excersice 4
length' :: [a] -> Int
length' [_] = 1
length' xs = 1 + length' (tail xs)

drop' :: Int -> [a] -> [a]
drop' 1 xs = tail xs
drop' n xs = drop' (n-1) (tail xs)

init' :: [a] -> [a] --all elements except the last one
init'[_] = []
init' (x:xs) = [x] ++ init' xs

-- excersice 5
and' :: [Bool] -> Bool
and' [x] = x
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' [x] = x
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 1 x = [x]
replicate' n x = (replicate' 1 x) ++ (replicate' (n-1) x)

getelement :: [a] -> Int -> a
getelement (x:xs) 0 = x
getelement (x:xs) n = getelement xs (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' e [] = False
elem' e (x:xs) = e == x || elem' e xs 


merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then
                        [x] ++ merge xs ([y]++ys) 
                        else
                            [y] ++ merge ([x]++xs) ys

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge fsthalve sndhalve
                where 
                    fsthalve = msort(fst(halve xs))
                    sndhalve = msort(snd(halve xs))
            
halve :: [a] -> ([a], [a])
halve a = (firsthalf, secondhalf)
  where
    firsthalf = take (length a `div` 2) a
    secondhalf = drop (length a `div` 2) a

sum' :: Num a => [a] -> a
sum' [] = 0
sum' [x] = x 
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 1 [] = []
take' 1 (x:xs) = [x]
take' n (x:xs) = [x] ++ take' (n-1) xs 

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last'(xs) 
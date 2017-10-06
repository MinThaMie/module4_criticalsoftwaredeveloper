-- Curried function: takes one argument and returns a function
-- add' :: Int -> (Int -> Int)
add' :: Num a => a -> a -> a -- Num is class constraint, a moet Typeclass Num zijn
add' x y = x + y

-- -> is right associative
-- int -> int -> int -> int
-- int -> (int -> (int -> int))

const :: a -> (b -> a)
const x = \_ -> x -- \ == Lambda experssion

--concat [x | xs <- xxs, x <- xs] -- eerst een lijst uit de lijst met lijsten, daarna een element uit de gekozen lijst

factors n = [x | x <- [1..n], n `mod` x == 0] -- list comprehension en guard expression en `` kan ook mod n x 

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product'(xs)

last' :: [a] -> a
last' a = head(reverse a)

init' :: [a] -> [a]
init' a = reverse(tail(reverse a))

bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2,3],[4,5,6]]

add'' :: Int -> Int -> Int -> Int
add'' x y z = x + y + z

copy :: a -> (a,a)
copy a = (a,a)

-- Types and classes 3
second xs = head (tail xs)
swap (x, y) = (y, x)
pair x y = (x, y)
double x = x * 2
palindrome xs = reverse xs == xs
twice f x = f (f x)
twice :: (a -> a) -> a -> a 
mapIncrementMap = map(map(+1)) [[1,2,3],[4,5]]

evenFilter = filter even[1..10] -- applies a function that maps a to bool to apply to a list

sumSqrEven :: [Int] -> Int
sumSqrEven ns = sum (map (^2) (filter even ns))
sumSqrEven' = sum . map (^2) . filter even

sum' :: [Int] -> Int
sum' = foldr (+) 0 -- foldr function limit (implicit on a list)

or' :: [Bool] -> Bool
or' = foldr (||) False -- replaces : with the operator, empty list by limit and right-association

length' :: [a] -> Int
length' = foldr(\_ n -> 1 + n ) 0  -- _ because we don't care about the x (element)

function xs = case length xs of 
            3 -> error "length 3"
            otherwise -> xs


-- Eq a => a
-- class Eq a where -- Something with the Type Eq needs to have two functions == and /=
--     (==), (/=) :: a -> a -> Bool
--     (/=) x y = not (x == y)
   

-- class Ord a => Eq a where
--     --(<), (=<) etc

-- data Bool = True | False deriving Show

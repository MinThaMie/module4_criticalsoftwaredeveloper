-- Exercise 1
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

multInt :: Int -> Int -> Int
multInt n 0 = 0
multInt n m = n + (multInt n (m-1))

addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ n) m = Succ (addNat n m)

multNat :: Nat -> Nat -> Nat
multNat n Zero = Zero
multNat (Succ n) m = addNat m (multNat m n)

-- Exercise 2
data Tree = Leaf Int | Node Tree Int Tree
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r

occursOrd m (Leaf n ) = compare m n == EQ
occursOrd m (Node l n r)    | compare m n == EQ = True
                            | compare m n == LT = occursOrd m l
                            | otherwise = occursOrd m r
-- Case statement is more efficient because it will only compare once
occursOrdCase m (Node l n r) = case compare m n of
                                EQ -> True
                                LT -> occursOrdCase m l
                                GT -> occursOrdCase r l 

flatten :: Tree -> [Int]
flatten (Leaf n) = [n]
flatten (Node l n r) = flatten l ++ [n] ++ flatten r

-- Exercise 3
data Boom  = Blaadje Int | Knoop (Boom) (Boom)
balance :: [Int] -> Boom
balance [x] = Blaadje x
balance xs = Knoop (balance firstHalve) (balance secondHalve)
                where 
                    firstHalve = take ((length xs) `div` 2 ) xs
                    secondHalve = drop ((length xs) `div` 2 ) xs

flatten'' ::  Boom -> [Int]
flatten'' (Blaadje x) = [x]
flatten'' (Knoop l r) = flatten'' l ++ flatten'' r

-- Exercise 4
numLeaves :: Boom -> Int
numLeaves (Blaadje x) = 1
numLeaves (Knoop l r) = numLeaves(l) + numLeaves(r)

balanced :: Boom -> Bool
balanced (Blaadje x) = True
balanced (Knoop l r) = stillBalanced && balanced l && balanced r
                          where 
                            stillBalanced = abs(numLeaves l  - numLeaves r) <= 1

boom1 = Knoop (Knoop (Blaadje 1) (Blaadje 4)) (Knoop (Blaadje 6) (Blaadje 9)) -- balanced
boom2 = Knoop (Knoop (Knoop (Blaadje 1) (Blaadje 2)) (Blaadje 3)) (Blaadje 4) -- not balanced
boom3 = Knoop (Knoop (Blaadje 1) (Blaadje 2)) (Blaadje 3) -- not balanced

-- Exercise 5
data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

expr1 = Val 2
expr2 = Val 6
expr3 = Add expr1 expr2
expr4 = Add expr3 expr3

f1 :: Int -> Int
f1 = (+2)

g1 :: Int -> Int -> Int
g1 = (+)

-- Exercise 6
eval :: Expr -> Int
eval x = folde id (+) x -- *1 == id

size :: Expr -> Int
size x = folde (const 1) (+) x

-- Exercise 7
-- instance Eq Maybe where
--     Nothing == Nothing  = True
--     (Just a) == (Just a) = a == a
--     _ == _ = False

-- instance Eq a => Eq (Maybe a) where
--     a == (Just a) = a == a
--     _ == Nothing = False




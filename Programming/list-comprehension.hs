sshunderd :: Int
sshunderd = sum[x ^ 2 | x <- [1..100]]

length' :: [a] -> Int
length' xs = sum[1 | x <- xs]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

square :: Int -> [(Int, Int)]
square x = [(x, y) | (x, y) <- grid x x, x /=y]

replicate':: Int -> a -> [a] 
replicate' n e = [e | x <- [1..n]]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [-n..n], y <- [-n..n], z <- [-n..n], x^2 + y^2 == z^2]

-- exercise 7
factors :: Int -> [Int]
factors n = [x | x <- [1..(n-1)], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], sum(factors x)==x]

-- exercise 8
result = [(x,y) | x <- [1,2], y <- [4,5]]
onegen = [(x, [y | y <- [4,5]]) | x <- [1,2]]


--exercise 9
scalarproduct ::[Int] -> [Int] -> Int
scalarproduct xs ys = sum[ x * y | (x,y) <- zip xs ys]
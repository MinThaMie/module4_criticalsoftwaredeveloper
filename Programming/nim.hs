-- 1: *****
-- 2: ****
-- 3: ***
-- 4: **
-- 5: *

import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r,n) <- zip [1..] board]
                      where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* ")) -- Concat because it's 5 * String 

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e
-- TODO Exercise 2
putBoardRec :: Board -> IO ()
putBoardRec xs = do putPartialBoardRec xs 1

putPartialBoardRec :: Board -> Int -> IO ()
putPartialBoardRec [x] row = do putRow row x
putPartialBoardRec (x:xs) row = do putRow row x
                                   putPartialBoardRec xs (row + 1)


-- Exercise 3
putBoardList :: Board -> IO ()
putBoardList b = sequence_ [ putRow x (getRow b x) | x <- [1..(length b)]]
getRow :: Board -> Int -> Int
getRow b n = b !! (n - 1)

newline :: IO ()
newline = putChar '\n'    

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     x <- getChar
                     getChar
                     newline
                     if isDigit x then
                        return (digitToInt x)
                     else 
                        do putStrLn "Doe niet zo raar creep"
                           getDigit prompt

play :: Board -> Int -> IO ()
play board player = 
    do newline
       putBoard board
       if finished board then
        do newline
           putStr "Speler "
           putStr (show (next player))
           putStrLn " heeft gewonnen!"
       else 
        do newline
           putStr "Speler "
           putStrLn (show player)
           row <- getDigit "Geef je rijnummer: "
           num <- getDigit "Geef aantal sterren: "
           if valid board row num then
            play (move board row num) (next player)
           else 
            do newline
               putStrLn "Dit is ook wel weer een beetje gek"
               play board player

nim :: IO ()
nim = play initial 1


adder :: IO ()
adder = do putStr "How many numbers?"
           amount <- getInt 
           if amount > 0 then
            do
              tot <- asker 0 amount
              putStrLn ("The total amount is " ++ (show tot))
           else
              do putStrLn "You're a creep"
                 adder

asker :: Int -> Int -> IO Int
asker tot 0 = return tot
asker tot n = do num <- getInt
                 newTot <- asker (tot + num) (n - 1)
                 return newTot

getInt :: IO Int
getInt = do num <- getLine
            return (read num :: Int)
asker' :: Int -> IO Int
asker' n = do nums <- (sequence [getInt | _ <- [1..n]])
              return (sum nums)

adder' :: IO ()
adder' = do putStr "How many numbers?"
            amount <- getInt 
            if amount > 0 then
             do
              tot <- asker' amount
              putStrLn ("The total amount is " ++ (show tot))
            else
              do putStrLn "You're a creep"
                 adder





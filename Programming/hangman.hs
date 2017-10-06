import System.IO 

hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetLine
             putStrLn "Try to guess it:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCharHidden
              if x == '\n' then
                do putChar x
                   return []
              else 
                do putChar '*'
                   xs <- sgetLine
                   return (x:xs)

getCharHidden :: IO Char 
getCharHidden = do hSetEcho stdin False
                   x <- getChar
                   hSetEcho stdin True
                   return x

play :: String -> IO ()
play word = do putStr "? "
               guess <- getLine
               if guess == word then
                putStrLn "You've got it \\o/"
               else 
                do putStrLn (match word guess)
                   play word

match :: String -> String -> String -- xs == word, ys == guess
match xs ys = [if elem x ys then x else '-' | x <- xs]

-- Exercise 6
readLineDel :: IO String 
readLineDel = helperReadLineDel ""

helperReadLineDel :: String -> IO String
helperReadLineDel xs = do x <- getCharHidden
                          if x == '\n' then
                            do putChar x
                               return xs
                          else 
                            if x == '\DEL' then
                                do putChar '\b' 
                                   helperReadLineDel (init xs)
                            else 
                                do putChar x
                                   helperReadLineDel ( xs ++ [x])





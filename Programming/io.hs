-- putChar :: a -> IO () Standard haskell function, implementation depends on OS
-- getChar :: IO Char 
import Prelude hiding (putStr, getLine)
putString :: String -> IO ()
putString [] = return ()
putString (x:xs) = do putChar x
                      putString xs


putStringLn :: String -> IO ()
putStringLn xs = do putString xs
                    putChar '\n'

getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                return []
             else
                do xs <- getLine
                   return (x:xs)
-- Exercise 1
putStr :: String -> IO ()
putStr xs = sequence_ [putChar x | x <- xs]

adds:: IO()
adds = do putStrLn "Insert the first value: "  
          one <- getLine  
          putStrLn "Insert the second value: "  
          two <- getLine    
          putStrLn "The result is:"
          print ((read one) + (read two))

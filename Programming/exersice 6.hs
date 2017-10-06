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
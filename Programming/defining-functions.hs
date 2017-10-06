halve :: [a]-> ([a], [a])
halve a = (firsthalf, secondhalf)
  where
    firsthalf = take (length a `div` 2) a
    secondhalf = drop (length a `div` 2) a

third:: [a] -> a
third a = head ( tail (tail a))
third' a = a !! 2
third'' (_:_:x:_) = x

safetail :: [a] -> [a]
safetail a = if null a
            then
                a
            else
                tail a

safetail' a | null a = a
            | otherwise = tail a

safetail'' (_:xs) = xs
safetail'' _ = []  

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

or'' False False = False
or'' _ _ = True

and'' x y = if (x == True)
                then if (y == True)
                        then  True
                    else False
            else False
and''' :: Bool -> Bool -> Bool
and''' x y = if (x == True)
                then y
            else False

-- Defining functions exec. 7
mult x y z = x * y * z
mult' x y = \z -> x * y * z
mult'' x = \y -> (\z -> x * y * z)
mult''' = \x -> (\y -> (\z -> x * y * z))

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (sum[luhnDouble w, x, luhnDouble y, z] `mod` 10) == 0

luhnDouble :: Int -> Int
luhnDouble x    | (x * 2) > 9 = (x*2) - 9
                | otherwise = x * 2

import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

shift :: Int -> Char -> Char
shift n c   | isLower c = int2let ((let2int c + n) `mod` 26)
            | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

percent :: Int -> Int -> Float
percent n m  = (fromIntegral n / fromIntegral m) * 100

lowers :: String -> Int
lowers cs = length [c | c <- cs, isLower c]

count :: Char -> String -> Int
count c cs = length [c' | c' <-cs, c == c']

freqs :: String -> [Float]
freqs cs = [percent (count c cs) n | c <- ['a'..'z']]
    where n = lowers cs

-- Frequentie tabel Nederlands wikipedia
dutch :: [Float]
dutch = [7.486, 1.584, 1.242, 5.993, 18.91,  0.805, 3.403, 2.380, 6.499,
         1.46,  2.248, 3.568, 2.213, 10.032, 6.063, 1.57,  0.009, 6.411,
         3.73,  6.79,  1.99,  2.85,  1.52,   0.036, 0.035, 1.39]  

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o - e) ^ 2 / e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = (drop n xs) ++ (take n xs)

posistions :: Eq a => a ->  [a] -> [Int]
posistions x xs = [i | (x' , i) <- zip xs [0..], x == x']

crack :: String -> String
crack xs = encode (-factor) xs
            where
                factor = head (posistions (minimum chitab) chitab)
                chitab = [chisqr (rotate n table') dutch | n <- [0..25]] 
                table' = freqs xs
                
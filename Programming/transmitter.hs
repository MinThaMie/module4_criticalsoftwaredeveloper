import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord) 

encodeParity :: String -> [Bit]
encodeParity = concat . map (parity . make8 . int2bin . ord) 

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

chop9 :: [Bit] -> [[Bit]] -- Is chop 8 but then using the unfold function
chop9 = unfold (== []) (take 9) (drop 9)


decode :: [Bit] -> String
decode = map(chr . bin2int) . chop8

decodeParity :: [Bit] -> String
decodeParity = map (chr. bin2int . parityCheck) . chop9

channel :: [Bit] -> [Bit]
channel = id

disruptedChannel :: [Bit] -> [Bit]
disruptedChannel xs | length xs < 64 = xs
                    | otherwise = drop 3 xs

transmit :: String -> String
transmit = decode . channel . encode

transmitParity :: String -> String
transmitParity = decodeParity . disruptedChannel . encodeParity

parity :: [Bit] -> [Bit]
parity xs| odd (sum[1 | x <-xs, x == 1]) = xs ++ [1]
         | otherwise = xs ++ [0]

parityCheck :: [Bit] -> [Bit]
parityCheck xs  | last (parity (take 8 xs)) == last xs = init xs
                | otherwise = error "Incorrect message" 

-- Exercise 9
unfold p h t x  | p x = []
                | otherwise = h x : unfold p h t (t x)

int2bin' = unfold (== 0) (`mod`2) (`div`2)

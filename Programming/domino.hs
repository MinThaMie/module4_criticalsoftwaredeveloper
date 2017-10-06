import Data.Char
import Data.List
import System.IO 
import Data.Maybe

type Grid   = [Square] -- List of squares
type Bone   = (Int, Pips) -- Number and dots
type Pips   = (Int, Int) -- The dots on a bone
type Square = (Int, Int) -- Value and posistion
type Pos    = (Int, Int)
type Stone = (Pips,Pos)
type Field = [Int]

bones :: [Bone]
bones = zip [1..] pips

pips :: [Pips]
pips = [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
         (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),(2,2),
         (2,3),(2,4),(2,5),(2,6),(3,3),(3,4),(3,5),
         (3,6),(4,4),(4,5),(4,6),(5,5),(5,6),(6,6)]

findBoneValue :: Pips -> [Int]
findBoneValue p = [ fst b | b <- bones, snd b == p ]

testGrid :: [[Int]]
testGrid = [[6,6,2,6,5,2,4,1],
            [1,3,2,0,1,0,3,4],
            [1,3,2,4,6,6,5,4],
            [1,0,4,3,2,1,1,2],
            [5,1,3,6,0,4,5,5],
            [5,5,4,0,2,6,0,3],
            [6,0,5,3,4,2,0,3]]

field :: Field
field = concat testGrid

realGrid :: Grid
realGrid = grid field

grid :: Field -> Grid
grid g = zip g [0..] 

neighbours :: Int -> Grid -> [Square]
neighbours n g = concat (map maybeToList ([upneigh n g] ++ [downneigh n g] ++ [leftneigh n g] ++ [rightneigh n g]))

upneigh :: Int -> Grid -> Maybe Square
upneigh n g = if (n - 8) > 0              then Just (g !! (n - 8)) else Nothing

downneigh n g = if (n + 8) < (length g)-1 then Just (g !! (n + 8)) else Nothing

leftneigh n g = if n `mod` 8 /= 0         then Just (g !! (n - 1)) else Nothing

rightneigh n g = if n `mod` 8 /= 7        then Just (g !! (n + 1)) else Nothing

horizontalOptions :: Grid -> [Stone]
horizontalOptions []        = []
horizontalOptions [x]       = []
horizontalOptions (x:y:sqs) | (fst x) > fst y = ((fst y, fst x),(snd y, snd x)) : horizontalOptions(y:sqs)
                            | otherwise = ((fst x, fst y),(snd x, snd y)) : horizontalOptions(y:sqs)

verticalOptions :: Grid -> [Stone]
verticalOptions g = [if (fst (g!!x)) > (fst (g !! (x + 8))) then ((fst (g!!(x + 8)), fst (g !! x)), (x , x + 8)) else ((fst (g!!x), fst (g !! (x + 8))), (x, x + 8)) | x <- [0..(length g)-9]] -- -1 because index 0 and -8 because of the last row

allOptions :: Grid -> [Stone]
allOptions g = (verticalOptions g) ++ (horizontalOptions g)

uniques :: [Stone] -> [Stone] -- Does not work on allOptions now
uniques [] = []
uniques (x:xs) | fst x `elem` (map fst xs) = uniques (filter ((/= (fst x)).fst) xs)
               | otherwise   = x : uniques xs

solution :: Field -> [Stone] -> Field
solution f []         = f
solution f (unq:unqs) = solution (tupleReplace unq f) unqs

tupleReplace :: Stone -> Field -> Field
tupleReplace s f = replaceAt (fst(snd s)) (findBoneValue (fst s)) (replaceAt (snd(snd s)) (findBoneValue (fst s)) f) 

replaceAt :: Int -> [Int] -> Field -> Field -- [Int] == bone number
replaceAt i v list = xs ++ v ++ ys
                     where (xs, _:ys) = splitAt i list


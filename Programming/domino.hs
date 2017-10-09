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

bones :: Int -> [Bone]
bones max = zip [1..] (pips max)

pips :: Int -> [Pips]
pips n = [(x,y) | x <- [0..n], y <- [x..n]]

findBoneValue :: Pips -> [Bone] -> [Int]
findBoneValue p bns = [ fst b | b <- bns, snd b == p ]

testGrid :: [[Int]]
testGrid = [[6,6,2,6,5,2,4,1],
            [1,3,2,0,1,0,3,4],
            [1,3,2,4,6,6,5,4],
            [1,0,4,3,2,1,1,2],
            [5,1,3,6,0,4,5,5],
            [5,5,4,0,2,6,0,3],
            [6,0,5,3,4,2,0,3]]

miniGrid :: [[Int]]
miniGrid =  [[0,1,1],
             [0,2,1],
             [0,2,2],
             [1,2,0]]

field :: Field
field = concat testGrid

minifield = concat miniGrid

realGrid :: Grid
realGrid = grid field

grid :: Field -> Grid
grid g = zip g [0..] 

horizontalOptions :: Grid -> [Stone]
horizontalOptions []        = []
horizontalOptions [x]       = []
horizontalOptions ((valx,posx):(valy,posy):sqs) | (posx `mod` 8 == 7) && (posy `mod` 8 == 0) = horizontalOptions((valy,posy):sqs)
                                                | valx > valy = ((valy, valx),(posy, posx)) : horizontalOptions((valy,posy):sqs)
                                                | otherwise = ((valx, valy),(posx, posy)) : horizontalOptions((valy,posy):sqs)

verticalOptions :: Grid -> [Stone]
verticalOptions g = [if (fst (g!!x)) > (fst (g !! (x + 8))) then ((fst (g!!(x + 8)), fst (g !! x)), ( x + 8, x)) 
                     else ((fst (g!!x), fst (g !! (x + 8))), (x, x + 8)) | x <- [0..(length g)-9]] -- -1 because index 0 and -8 because of the last row

allOptions :: Grid -> [Stone]
allOptions g = (verticalOptions g) ++ (horizontalOptions g)

uniques :: [Stone] -> [Stone] -- Does not work on allOptions now
uniques [] = []
uniques (((val1,val2),y):xs) | (val1,val2) `elem` (map fst xs) = uniques (filter ((/= (val1,val2)).fst) xs)
               | otherwise   = ((val1,val2),y) : uniques xs

solution :: Field -> [Stone] -> [Bone] -> Field
solution f [] bns     = f
solution f (unq:unqs) bns = solution (tupleReplace unq bns f) unqs bns

tupleReplace :: Stone -> [Bone] -> Field -> Field
tupleReplace ((val1, val2), (pos1, pos2)) bns f = replaceAt pos1 (findBoneValue bone bns) (replaceAt pos2 (findBoneValue bone bns) f)
                                              where bone = (val1, val2) 

replaceAt :: Int -> [Int] -> Field -> Field -- [Int] == bone number
replaceAt i v list = xs ++ v ++ ys
                     where (xs, _:ys) = splitAt i list

removeUsedSquares :: Stone -> [Stone] -> [Stone]
removeUsedSquares stone [] = []
removeUsedSquares ((val1,val2), (pos1, pos2)) (((valx, valy),(posx, posy)):stns)| pos1 == posx || pos1 == posy || pos2 == posy || pos2 == posx = removeUsedSquares ((val1,val2), (pos1, pos2)) stns
                                                                                | otherwise = ((valx,valy),(posx,posy)) : removeUsedSquares ((val1,val2), (pos1, pos2)) stns

removeUsedValues :: Stone -> [Stone] -> [Stone] 
removeUsedValues stone [] = []
removeUsedValues ((val1,val2), (pos1, pos2)) (((valx, valy),(posx, posy)):stns)| val1 == valx && val2 == valy || val1 == valy && val2 == valx = removeUsedValues ((val1,val2), (pos1, pos2)) stns
                                                                               | otherwise = ((valx,valy),(posx,posy)) : removeUsedValues ((val1,val2), (pos1, pos2)) stns
                      
removeAllUsedSquares :: [Stone] -> [Stone] -> [Stone]
removeAllUsedSquares [] opts = opts
removeAllUsedSquares _ [] = []
removeAllUsedSquares (rmv:rmvs) opts = removeAllUsedSquares rmvs (removeUsedSquares rmv opts)

removeAllUsedValues :: [Stone] -> [Stone] -> [Stone]
removeAllUsedValues [] opts = opts
removeAllUsedValues _ [] = []
removeAllUsedValues (rmv:rmvs) opts = removeAllUsedValues rmvs (removeUsedValues rmv opts)

removeFromBones :: Pips -> [Bone] -> [Bone]
removeFromBones pip bns = filter ((/= pip).snd) bns

removeStonesFromBones :: [Stone] -> [Bone] -> [Bone]
removeStonesFromBones [] bns = bns
removeStonesFromBones (((val1, val2), _) : stns) bns = removeStonesFromBones stns (removeFromBones (val1,val2) bns)

solve :: Field -> Int -> IO()
solve f max = putStr . unlines $ map show (chop 3 (solve' (bones max) (allOptions (grid f)) f))

solve' :: [Bone] -> [Stone] -> Field -> Field 
solve' b [] f = f
solve' b (opt:opts) f | null b && null opts = f
                      | null b || null opts = f
                      | null (uniques opts) = solve' (removeStonesFromBones [opt] b) (removeAllUsedValues [opt] (removeAllUsedSquares [opt] opts)) (solution f [opt] b)
                      | otherwise =  solve' (removeStonesFromBones (uniques (opt:opts)) b) (removeAllUsedSquares (uniques (opt:opts)) opts) (solution f (uniques (opt:opts)) b)

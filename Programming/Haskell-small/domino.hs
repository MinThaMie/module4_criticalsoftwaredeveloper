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

field :: Field
field = [0,1,1,0,2,1,0,2,2,1,2,0]

grid :: Field -> Grid
grid g = zip g [0..] 

horizontalOptions :: Grid -> [Stone]
horizontalOptions []        = []
horizontalOptions [x]       = []
horizontalOptions ((valx,posx):(valy,posy):sqs) | (posx `mod` 3 == 2) && (posy `mod` 3 == 0) = horizontalOptions((valy,posy):sqs)
                                                | valx > valy = ((valy, valx),(posy, posx)) : horizontalOptions((valy,posy):sqs)
                                                | otherwise = ((valx, valy),(posx, posy)) : horizontalOptions((valy,posy):sqs)

verticalOptions :: Grid -> [Stone]
verticalOptions g = [if (fst (g!!x)) > (fst (g !! (x + 3))) then ((fst (g!!(x + 3)), fst (g !! x)), ( x + 3, x)) 
                     else ((fst (g!!x), fst (g !! (x + 3))), (x, x + 3)) | x <- [0..(length g)-4]] -- -1 because index 0 and -8 because of the last row

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
removeUsedValues ((val1,val2), (pos1, pos2)) (((valx, valy),(posx, posy)):stns)| val1 == valx && val2 == valy = removeUsedValues ((val1,val2), (pos1, pos2)) stns
                                                                               | otherwise = ((valx,valy),(posx,posy)) : removeUsedValues ((val1,val2), (pos1, pos2)) stns
                      
removeAllUsedSquares :: [Stone] -> [Stone] -> [Stone]
removeAllUsedSquares [] opts = opts
removeAllUsedSquares _ [] = []
removeAllUsedSquares (rmv:rmvs) opts = removeAllUsedSquares rmvs (removeUsedSquares rmv opts)

removeAllUsedValues :: [Stone] -> [Stone] -> [Stone]
removeAllUsedValues [] opts = opts
removeAllUsedValues _ [] = []
removeAllUsedValues (rmv:rmvs) opts = removeAllUsedValues rmvs (removeUsedValues rmv opts)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

data Tree a = Node a [Tree a] deriving Show

move :: Field -> Stone -> Bone -> Field
move f ((val1, val2), (pos1, pos2)) b = replaceWith pos1 (fst b) (replaceWith pos2 (fst b) f)
                                        where replaceWith i v list = xs ++ [v] ++ ys
                                                                     where (xs, _:ys) = splitAt i list

moves :: Field -> Bone -> [Stone] -> [(Field, [Stone])]
moves f b opts = [(move f optB b, (removeUsedSquares optB (removeUsedValues optB opts)))| optB <- opts, (snd b) == (fst optB)]

gametree :: (Field,[Stone]) -> [Bone] -> Tree (Field) -- Pass options to gametree because you can then remove the options
gametree (f,opts) [] = Node f []
gametree (f,opts) (b : bns) = Node f [gametree (f',opts') bns | (f', opts') <- moves f b opts]

allBonesUsed :: Field -> Bool
allBonesUsed f = null ((f \\ [1..28])\\[1..28]) 

solutions :: Tree (Field) -> [Field]
solutions (Node f []) = if allBonesUsed f then [f] else [] 
solutions (Node a (t:ts)) = solutions t ++ solutions (Node a ts)

showSolutions :: [Field] -> IO ()
showSolutions fs = sequence_ [printField f | f <- fs] 

printField :: Field -> IO();
printField f = putStrLn . unlines $ map showRow (chop 3 f)

showRow :: [Int] -> String
showRow = concat . map showNum

showNum :: Int -> String
showNum n | n < 10    = "  " ++ show n ++ " "
          | otherwise = " " ++ show n ++ " "

main :: IO ()
main = showSolutions(solutions(gametree (field, allOptions(grid field)) (bones 2) ))

leafTree :: Tree(Field) -> [Field]
leafTree (Node f []) = [f]
leafTree (Node a (t:ts)) = leafTree t ++ leafTree (Node a ts)

countTree :: Tree(Field) -> Int
countTree (Node f []) = 1
countTree (Node a (t:ts)) = countTree t + countTree (Node a ts)

countLeafs :: Tree Field -> [Int] 
countLeafs (Node b []) = [1]
countLeafs (Node b ts) = [b' | ts' <- ts, b' <- countLeafs ts']

counter :: Tree Field -> Int
counter = sum . countLeafs
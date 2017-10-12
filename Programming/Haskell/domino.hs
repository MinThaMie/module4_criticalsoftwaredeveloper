import Data.Char
import Data.List

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

field :: Field
field = [6,6,2,6,5,2,4,1,
         1,3,2,0,1,0,3,4,
         1,3,2,4,6,6,5,4,
         1,0,4,3,2,1,1,2,
         5,1,3,6,0,4,5,5,
         5,5,4,0,2,6,0,3,
         6,0,5,3,4,2,0,3]

field2 :: Field
field2 = [5,4,3,6,5,3,4,6,
          0,6,0,1,2,3,1,1,
          3,2,6,5,0,4,2,0,
          5,3,6,2,3,2,0,6,
          4,0,4,1,0,0,4,1,
          5,2,2,4,4,1,6,5,
          5,5,3,6,1,2,3,1]

field3 :: Field 
field3 = [4,2,5,2,6,3,5,4,
          5,0,4,3,1,4,1,1,
          1,2,3,0,2,2,2,2,
          1,4,0,1,3,5,6,5,
          4,0,6,0,3,6,6,5,
          4,0,1,6,4,0,3,0,
          6,5,3,6,2,1,5,3]


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

removeUsedSquares :: Stone -> [Stone] -> [Stone]
removeUsedSquares stone [] = []
removeUsedSquares ((val1,val2), (pos1, pos2)) (((valx, valy),(posx, posy)):stns)| pos1 == posx || pos1 == posy || pos2 == posy || pos2 == posx = removeUsedSquares ((val1,val2), (pos1, pos2)) stns
                                                                                | otherwise = ((valx,valy),(posx,posy)) : removeUsedSquares ((val1,val2), (pos1, pos2)) stns

removeUsedValues :: Stone -> [Stone] -> [Stone] 
removeUsedValues stone [] = []
removeUsedValues ((val1,val2), (pos1, pos2)) (((valx, valy),(posx, posy)):stns)| val1 == valx && val2 == valy = removeUsedValues ((val1,val2), (pos1, pos2)) stns
                                                                               | otherwise = ((valx,valy),(posx,posy)) : removeUsedValues ((val1,val2), (pos1, pos2)) stns

data Tree a = Node a [Tree a] deriving Show

move :: Field -> Stone -> Bone -> Field
move f ((val1, val2), (pos1, pos2)) b = replaceWith pos1 (fst b) (replaceWith pos2 (fst b) f)
                                        where replaceWith i v list = xs ++ [v] ++ ys
                                                                     where (xs, _:ys) = splitAt i list

moves :: Field -> Bone -> [Stone] -> [(Field, [Stone])]
moves f b opts = [(move f optB b, (removeUsedSquares optB (removeUsedValues optB opts)))| optB <- opts, (snd b) == (fst optB)]

gametree :: (Field,[Stone]) -> [Bone] -> Tree (Field) -- Pass options to gametree because you can then remove the options
gametree (f,opts) [] = Node (f) []
gametree (f,opts) (b : bns) = Node (f) [gametree (f',opts') bns | (f', opts') <- moves f b opts]

allBonesUsed :: Field -> Bool
allBonesUsed f = null ((f \\ [1..28])\\[1..28]) 

solutions :: Tree (Field) -> [Field]
solutions (Node (f) []) = if allBonesUsed f then [f] else [] 
solutions (Node a (t:ts)) = solutions t ++ solutions (Node a ts)

showSolutions :: [Field] -> IO ()
showSolutions fs = sequence_ [printField f | f <- fs] 

printField :: Field -> IO();
printField f = putStrLn . unlines $ map showRow (chop 8 f)

showRow :: [Int] -> String
showRow = concat . map showNum

showNum :: Int -> String
showNum n | n < 10    = "  " ++ show n ++ " "
          | otherwise = " " ++ show n ++ " "

main :: IO ()
main = showSolutions(solutions(gametree (field, allOptions(grid field)) (bones 6) ))



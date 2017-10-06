-- getallen: 1 3 7 10 20 50
-- doel: 765
-- oplossing: (1+50)*(25-10)
-- expr1 = App Add (Val 1) (App Mul (Val 2) (Val 3))
data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show Exp = "^"


validBruteForce :: Op -> Int -> Int -> Bool
validBruteForce Add _ _ = True  
validBruteForce Sub x y = x > y
validBruteForce Mul _ _ = True
validBruteForce Div x y = x `mod` y == 0
validBruteForce Exp _ _ = True

valid :: Op -> Int -> Int -> Bool -- Optimized
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y 
valid Div x y = y > 1 && x `mod` y == 0
valid Exp x y = x > 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = paren l ++ show o ++ paren r
                        where 
                            paren (Val n) = show n
                            paren e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n)      = [n]
values (App _ l r)  = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)        = [n | n > 0]
eval (App o l r)    = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map(x:) yss
                where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []        = [[]]
perms (x:xs)    =  concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs
choicesList :: [a] -> [[a]]
choicesList xs = [ chs | sbs <- subs xs, chs <- perms sbs]

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys  = elem x ys && isChoice xs (rmfirstOcc x ys)

rmfirstOcc :: Eq a => a -> [a] -> [a]
rmfirstOcc x [] = []
rmfirstOcc x (y:ys) = if x /= y then
                        [y] ++ rmfirstOcc x ys 
                      else 
                        ys

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/= x) xs) 

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a],[a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x],xs) : [(x:ls, rs) | (ls, rs) <- split xs]
-- Generalizing this will break, because you cant do combine on a empty list
-- Allowing empty lists to split, creates infinite options and also a list with one element into a pair of ([x],[])
-- will also create a problem in exprs

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [ e  | (ls, rs) <- split ns,
                 l <- exprs ls,
                 r <- exprs rs,
                 e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

main :: IO() -- Lege haakjes betekent geen resultaat
main = print(solutionsRes [1,3,7,10,25,50] 765)

type Result = (Expr, Int)

combineRes :: Result -> Result -> [Result]
combineRes (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n)]
results ns = [ e  | (ls, rs) <- split ns,
                 l <- results ls,
                 r <- results rs,
                 e <- combineRes l r]

solutionsRes :: [Int] -> Int -> [Expr]
solutionsRes ns n = [e | ns' <- choices ns, (e, m) <- results ns', m == n]


possibleExpressions :: [Int] -> Int
possibleExpressions xs = length ([ y | x <- choices xs, y <- exprs x])

evalPossibleExpressions :: [Int] -> Int
evalPossibleExpressions xs = length ([ vld | chs <- choices xs, ex <- exprs chs, vld <- eval ex])










import Control.Concurrent

cls :: IO () -- clear screen
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H") -- puts cursor on posistion

width :: Int
width = 20

height :: Int
height = 20

type Board = [Pos]

glider :: Board
glider = [(4,2),(2,3),(4,3),(3,4),(4,4)]
ossi :: Board
ossi = [(2,0),(3,0),(4,0),(8,0),(9,0),(10,0),
        (0,2),(5,2),(7,2),(12,2),
        (0,3),(5,3),(7,3),(12,3),
        (0,4),(5,4),(7,4),(12,4),
        (2,5),(3,5),(4,5),(8,5),(9,5),(10,5),
        (2,7),(3,7),(4,7),(8,7),(9,7),(10,7),
        (0,8),(5,8),(7,8),(12,8),
        (0,9),(5,9),(7,9),(12,9),
        (0,10),(5,10),(7,10),(12,10),
        (2,12),(3,12),(4,12),(8,12),(9,12),(10,12)]

ossi2 = [(x + 2, y + 2) | (x, y) <- ossi ]

spaceship :: Board
spaceship = [(0,0),(1,0),(2,0),(3,0),(4,0),(5,0),(0,1),(6,1),(0,2),(1,3),(3,4),(4,4),(6,3)]

spaceship2 :: Board
spaceship2 = [(x+5,y+5) | (x,y) <- spaceship]

oscilator :: Board
oscilator = [(x + 5,5) | x <- [1..10]]

flipflop :: Board
flipflop = [(x + 2,y + 2) | x <- [1..3], y <- [1..3]] ++ [(x + 5,y + 5) | x <- [1..3], y <- [1..3]]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b 

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1, y-1), (x, y-1), (x+1, y-1), 
                          (x-1, y),             (x+1, y), 
                          (x-1, y+1), (x, y+1), (x+1, y+1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x-1) `mod` width) + 1, ((y-1) `mod` height) +1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/= x) xs)

births :: Board -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)), 
                    isEmpty b p,
                    liveneighbs b p == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showcells b
            threadDelay 500000
            life (nextgen b)

















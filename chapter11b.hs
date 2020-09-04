import           Data.Char
import           Data.List
import           System.IO

size :: Int
size = 3

type Grid = [[Player]]

data Player = SO | O | B | X | GX
    deriving (Eq, Ord, Show)

next :: Player -> Player
next O  = X
next B  = B
next SO = SO
next X  = O
next GX = GX

empty :: Grid
empty = replicate size (replicate size B)

exampleGrid :: Grid
exampleGrid = [[X,B,O],[B,O,O],[X,X,B]]

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player -> Player
turn g sp = if os <= xs then sp else next sp
         where
             os = length (filter (== sp) ps)
             xs = length (filter (== next sp) ps)
             ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
               line = all (== p)
               rows = g
               cols = transpose g
               dias = [diag g, diag (map reverse g)]


win_size :: Int
win_size = 3

wins' :: Player-> Grid -> Bool
wins' p g = any (checkline (p) win_size) (rows ++ cols ++ dias)
           where
             rows = g
             cols = transpose g
             dias = diags g

checkline :: Player -> Int -> [Player] -> Bool
checkline p a xs = a <=  maximum ( 0: (map length ys) )
                       where spx = splitPlayer (next p) xs
                             ys = concat $map (splitPlayer B) spx
splitPlayer :: Ord a => a -> [a] -> [[a]]
splitPlayer p xs = if y == [] then ys else (y:ys)
                   where (y:ys) = splitPlayer' xs p

splitPlayer' :: Ord a => [a] -> a -> [[a]]
splitPlayer' [] p = [[]]
splitPlayer' (x:xs) p = if x == p
                       then if head spx /= [] then [] : spx else spx
                       else (x: head spx) : tail spx
                       where spx = splitPlayer' xs p


diagPos :: [[(Int,Int)]]
diagPos = [zip [xs..size-1][0..size-1]|  xs <- [0..size-1] ] ++ [zip [0..size-1] [xs..size-1]| xs <- [1..size-1]]

diags :: Grid -> [[Player]]
diags g = [[ g !! (fst pos) !! (snd pos) | pos <- xs]| xs <- diagPos] ++
          [[ reverse g !! (fst pos) !! (snd pos) | pos <- xs]| xs <- diagPos]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Grid -> Bool
won g = wins' O g || wins' X g

putGrid :: Grid -> IO()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p =
    if valid g i then [chop size (xs ++ [p] ++ ys)] else []
    where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                       return (read xs - 1)
                   else
                       do putStrLn "Error: Invalid number"
                          getNat prompt


prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr  ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]


moves :: Grid -> Player -> [Grid]
moves g p
   | won g  = []
   | full g = []
   | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

type Decisiontree = Tree (Grid, Player)
type Ratedtree = Tree(Grid, Player, Player, Player)

minimax :: Player -> Tree Grid  -> Decisiontree
minimax p tree@(Node g [])
  | wins' O g = Node (g,O) []
  | wins' X g  = Node (g,X) []
  | otherwise = Node (g,B) []
minimax p (Node g ts)
  | turn g p == O = Node (g, minimum ps) ts'
  | turn g p == X = Node (g, maximum ps) ts'
                  where
                      ts' = map (minimax p) ts
                      ps = [pl | Node (_,pl) _ <- ts']


nextmoves :: Player -> Player -> Player -> [Grid] -> [Decisiontree]
nextmoves _ _ _ [] = []
nextmoves sp alpha beta (m:ms) = case sp of
                                                         X -> if beta < p
                                                                 then [tree]
                                                                 else tree : ( nextmoves sp (max p alpha) beta ms)
                                                                 where
                                                                     tree@(Node(_,p) _) = ratetree (next sp) alpha beta m
                                                         O -> tree : nextmoves sp alpha (min p beta) ms
                                                                 --if alpha > p
                                                                 --then [tree]
                                                                 --else tree : (nextmoves sp alpha (min p beta) ms )
                                                                 where
                                                                       tree@(Node(_,p) _) = ratetree (next sp) alpha  beta m

ratetree :: Player -> Player -> Player -> Grid -> Decisiontree
ratetree sp alpha beta g | wins' O g = Node(g,O) []
                                      | wins' X g = Node(g,X) []
                                      | full g = Node(g,B) []
                                      | sp == O = Node (g, minvalue)  ratedtrees
                                      | sp == X = Node (g, maxvalue) ratedtrees
                                      where
                                             ms = moves g sp
                                             ratedtrees = nextmoves sp alpha beta ms
                                             minvalue = minimum $ map(\(Node (_,p) _) -> p) ratedtrees
                                             maxvalue = maximum $ map(\(Node (_,p) _) -> p) ratedtrees



bestmove :: Decisiontree -> Player -> Decisiontree
bestmove tree@(Node (g,best) ts)  p = head [Node (g',p') ts' | Node (g', p') ts' <- ts, p' == best]

play :: Decisiontree -> Player -> IO ()
play (Node (g,p') t) p = do
    putStr "\ESC[2J"
    goto(1,1)
    putGrid g
    play' (Node (g,p') t) p


treeByGrid :: Grid -> Decisiontree -> Decisiontree
treeByGrid  g (Node _ ts) = treeByGrid' g ts

treeByGrid' :: Grid -> [Decisiontree] -> Decisiontree
treeByGrid'  g ((Node (g',p) t):ts) = if g == g'
                                                      then (Node (g',p)  t)
                                                      else treeByGrid' g ts

play' :: Decisiontree -> Player -> IO ()
play' tree@(Node (g,p') ts)  p
    | wins' O g = putStrLn "Player O wins!\n"
    | wins' X g = putStrLn "Player X wins!\n"
    | full g   = putStrLn "It's a draw!\n"
    | p == O   = do i <- getNat (prompt p)
                    case move g i p of
                        [] -> do putStrLn "ERROR: Invalid move"
                                 play' tree  p
                        [g'] -> play (treeByGrid g' tree)  (next p)
    | p == X   = do putStr "Player X is thinking... "
                    (play $! (bestmove tree p)) (next p)



decidePlayer :: IO Player
decidePlayer = do putStr "You want to play first (y,n): "
                  c <- getLine
                  if c == "y"
                    then return O
                    else if c == "n"
                         then return X
                         else do putStrLn "Wrong input please repeat!"
                                 decidePlayer

numberOfNodes :: Decisiontree -> Int
numberOfNodes (Node _ ts) = 1 + sum (map numberOfNodes ts)

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          x <- decidePlayer
          play (ratetree x SO GX empty) x
          --play (minimax x (gametree empty x) ) x

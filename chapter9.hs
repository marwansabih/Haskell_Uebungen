data Op = Add | Mul | Sub | Div | Exp

instance Show Op where
  show Add = "+"
  show Mul = "*"
  show Sub = "-"
  show Div = "/"
  show Exp = "^"

apply :: Op -> Int -> Int -> Int
apply Add x y = x+y
apply Mul x y = x*y
apply Sub x y = x-y
apply Div x y = x `div` y
apply Exp x y = x^y

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Sub x y = x > y
valid Div x y = y /= 1 && mod x y == 0 && y /= 0
valid Exp x y = y > 1 && x /= 1 && abs x < 5 && abs y < 5

data Expr= Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
    where brak (Val n) = show n
          brak e       = "(" ++ show e ++ ")"

e1 :: Expr
e1 = App Add (Val 5) (App Mul (Val 3) (Val 4))

values :: Expr -> [Int]
values (Val n)     = [n | n > 0]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]



subs :: [Int] -> [[Int]]
subs []     = [[]]
subs (x:xs) =  (subs xs) ++ map (x:) (subs xs)

interleave :: Int -> [Int] -> [[Int]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : [y:es| es <- interleave x ys ]

perms :: [Int] -> [[Int]]
perms []     = [[]]
perms (x:xs) = concat $ map (interleave x) $ perms xs

choices = concat . (map perms) . subs

choices' xs = [c | sub <- subs xs, c  <- perms sub ]

split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [ (x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns,
                l <- exprs ls,
                r <- exprs rs,
                e <- combine l r]

ops:: [Op]
ops = [Add, Mul, Sub, Div, Exp]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | nss <- choices ns, e <- exprs nss, eval e == [n]]

type Result = (Expr,Int)

measure :: Expr -> (Int,Int)
measure e = (length (values e), sum (values e) )

instance Eq Expr where
  e2 == e3 = measure e2 == measure e3

instance Ord Expr where
  e2 < e3 = measure e2 < measure e3
  e2 <= e3 = measure e2 < measure e3 || measure e2 == measure e3
  e2 > e3 = measure e2 > measure e3
  e2 >= e3 = measure e2 > measure e3 || measure e2 == measure e3



results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n,n)|n > 0]
results ns = [res | (ls,rs) <- split ns,
  lx <- results ls,
  ry <- results rs,
  res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m==n]

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort smaller ++ [x] ++ quicksort larger
                     where smaller = filter (<=x) xs
                           larger  = filter (>x) xs

remove :: Eq a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys) = if x == y
               then ys
               else [y] ++ ( remove x ys)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = let rs = remove x ys
                     in if length rs == length ys
                         then False
                         else isChoice xs ys




inc_solutions' xs n inc = solutions' xs (n+inc) ++ solutions' xs (n-inc)

nearest_solutions xs n inc = case s of
                           [] -> inc_solutions' xs n (inc +1)
                           _  -> s
                          where s = inc_solutions' xs n inc

main :: IO()
main = do
  print $ e1
  print $ eval e1
  print $ choices [1,2,3]
  --print $ solutions [1,3,7,10,25,50] 765
  --print $ quicksort $ solutions' [3,5,11,14, 33, 44] 55
  --print $ quicksort $ solutions [3,5,11,14, 33, 44] 55
  --print $ solutions' [1,3,7,10,25,50] 765
  print $ quicksort [3,1,4,8,2,1]
  print $ remove 3 [5,4,3,5,4]
  print $ remove 3 [4,5,4]
  print $ isChoice [3,2] [4,5,6,3,8,2]
  print $ isChoice [3,2] [4,3,7,8,9]
  print $ choices' [1,2,3]
  print $ nearest_solutions [2,3] 7 0

data Nat = Zero | Succ Nat deriving Show

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _     = Zero
mult (Succ m) n = add n (mult m n)

data Tree a = Leaf a | Node (Tree a) a (Tree a)

t:: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
    (Node (Leaf 6) 7 (Leaf 9))

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) | x == y = True
                      | x < y  = occurs x l
                      | otherwise = occurs x r

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y)     = x == y
occurs2 x (Node l y r) = case compare x y of
                         EQ -> True
                         LT -> occurs x l
                         GT -> occurs x r

data Bree a = Beaf a | Bode (Bree a) (Bree a) deriving Show

b = Bode (Bode (Beaf 10) (Bode (Beaf 10)(Beaf 12))) (Beaf 11)

numberBeaf :: Bree a -> Int
numberBeaf (Beaf a)     = 1
numberBeaf (Bode b1 b2) = numberBeaf b1 + numberBeaf b2

balanced :: Bree a -> Bool
balanced (Beaf _ ) = True
balanced (Bode b1 b2) = abs (n1 - n2) <= 1
                    &&   balanced b1
                    &&   balanced b2
                    where n1 = numberBeaf b1
                          n2 = numberBeaf b2

halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
           where half = length xs `div` 2

halve' :: [a] -> ([a],[a])
halve' xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Bree a
balance [x] = Beaf x
balance xs = Bode (balance h1) (balance h2)
            where (h1,h2) = halve xs



data Expr = Val Int | Add Expr Expr | Mul Expr Expr

folde :: (Int -> a) -> (a -> a -> a ) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add a b) = g (folde f g a) (folde f g b)

eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (const 1) (+) e

e = Add (Val 5) (Add (Val 3) (Val 2))

data Maybe' a = Nothing' | Just' a

instance Eq a => Eq (Maybe' a) where
  Just' x == Just' y = x == y
  Just' _ == Nothing' = False
  Nothing' == Just' _ = False
  Nothing' == Nothing' = True

data List a = Data [a]

instance Eq a => Eq (List a) where
  Data xs == Data ys | length ys /= length xs = False
                     | otherwise  = and $ zipWith(==) xs ys


d :: List Int
d = Data []

data Prop = Const Bool
           | Var Char
           | Not Prop
           | And Prop Prop
           | Or Prop Prop
           | Imply Prop Prop
           | Equal Prop Prop

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

eval2 :: Subst -> Prop -> Bool
eval2 _ (Const b)   = b
eval2 s (Var x)     = find x s
eval2 s (Not p)     = not (eval2 s p)
eval2 s (And p q)   = eval2 s p && eval2 s q
eval2 s (Or p q)    = eval2 s p || eval2 s q
eval2 s (Imply p q) = eval2 s p <= eval2 s q
eval2 s (Equal p q) = eval2 s p == eval2 s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Equal p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
          where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
         where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval2 s p | s <- substs p]

p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'B') (Var 'b'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
p5 = Or (Var 'A') (Not (Var 'A'))

p6 :: Prop
p6 = Equal (Var 'A') (Var 'A')



type Cont = [Op]

data Op = EVALA Expr | EVALM Expr | ADD Int | MUL Int

evalA :: Expr -> Cont -> Int
evalA (Val n)   c = exec c n
evalA (Add x y) c = evalA x (EVALA y : c)
evalA (Mul x y) c = evalA x (EVALM y : c)

exec :: Cont -> Int -> Int
exec []           n  = n
exec (EVALA y : c) n = evalA y (ADD n : c)
exec (EVALM y : c) n = evalA y (MUL n : c)
exec (ADD n : c)  m  = exec c (n+m)
exec (MUL n : c)  m  = exec c (n*m)

value :: Expr -> Int
value e = evalA e []


main :: IO()
main = do
  print "Chapter 8"
  print $ value (Add (Val 6) (Add (Val 3) (Val 4)))
  print $ value (Mul (Val 6) (Add (Val 3) (Val 4)))
  print $ value (Add (Val 6) (Mul (Val 3) (Val 4)))

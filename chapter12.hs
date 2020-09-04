data Tree1 a = Leaf1  | Node1 (Tree1 a) a (Tree1 a)
         deriving Show

data Pair a b = P a b
         deriving Show

afun :: (a -> b -> (Pair a b)) -> Maybe a -> Maybe b -> Maybe (Pair a b)
afun g x y = g <$> x <*> y

instance Functor Tree1 where
  -- fmap :: (a->b) -> Tree a -> Tree b
  fmap g (Leaf1)         = Leaf1
  fmap g (Node1 t1 a t2) = Node1 (fmap g t1) (g a) (fmap g t2)

pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys = do
                          x <- xs
                          y <- ys
                          return (x,y)

--instance Functor ((->) a) where
  --fmap :: (b->c) -> (a->b) -> (a-> c)
  --fmap  = (.)

  --instance Aplicative ((->)a) where
    --pure :: b -> (a -> b)
    -- pure = const
    -- <*> (a->b->c)->(a->b)->(a->c)
    -- f <*> g = (\x -> (f x) (g x))

--instance Monad ((->)a) where
  -- (>>=) (a->b) ->(b->(a->c))->(a->c)
  -- f >>= g = (\x -> g (f  x) x)

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
  --fmap :: (a-> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z (repeat x)
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z [g x | (g,x) <- zip gs xs]

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

instance Functor Expr where
  --fmap :: (a->b) -> Expr a -> Expr b
  fmap  g (Val x)          = Val x
  fmap g  (Var x)          = Var (g x)
  fmap g (Add expr1 expr2) = Add (fmap g expr1) (fmap g expr2)

instance Applicative Expr where
    -- pure :: a->Expr a
    pure x = Var x
    -- (<*>) :: (Expr (a->b) -> Expr a -> Expr b)
    expr <*> (Val x) = Val x
    (Var g) <*> (Var x) = Var (g x)
    (Val g) <*> (Var x) = Val g
    (Add expr1 expr2) <*> expr = Add (expr1 <*> expr) (expr2 <*> expr)
    expr <*> (Add expr1 expr2) = Add (expr <*> expr1) (expr <*> expr2)

instance Monad Expr where
  --(>>=) :: Expr a -> (a-> Expr b) -> Expr b
  Val x >>= f = Val x
  Var x >>= f = f x
  Add expr1 expr2 >>= f = Add (expr1 >>= f) (expr2 >>= f)

h :: Char -> Expr String
h x = Add (Var ['a',x,'c']) (Val 5)

expr1 = Add (Var 'a') (Add (Val 2) (Var 'b'))
expr2 = Add (Var (\x -> x:[])) (Add (Val 2) (Var (\x -> 'a':x:[])))
--expr2 = Var(\x -> x:[])

type State = Int

data ST a = S (State -> (a, State))

app :: ST a -> (State -> (a, State))
app (S x) = x

instance Functor ST where
  --fmap :: (a- > b) -> ST a -> ST b
  fmap g st = do
                      x <- st
                      return (g x)
instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S(\s -> (x,s))
  --(<*>) :: ST (a -> b) -> ST a -> ST b
  stf  <*> stx = do
                         f <- stf
                         x <- stx
                         return (f x)

instance Monad ST where
  -- (>>=) :: ST a -> (a -> ST b) -> ST b
  st >>= f =  S (\s -> let (x,s') = app st s in  app (f x) s')


data Tree a = Leaf  a | Node (Tree a)  (Tree a)
           deriving Show

tree = Node ( Node (Leaf  'a') (Leaf  'b') )  (Leaf  'c')

fresh :: ST Int
fresh = S (\n -> (n,n*2))


alabel :: Tree a -> ST (Tree Int)
alabel (Leaf _)   = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r

main = do
              print $ fmap (*2)  $ Node1 (Node1 Leaf1 10 Leaf1) 11  (Leaf1)
              print $  (+) <$> [2,3,4,5] <*> [1,2,3]
              print $ (pure (.) <*> [(2+),(3+), (4+)] <*> [(2*),(3*),(4*)]) <*> [1,2,3]
              print $ [(2+),(3+), (4+)] <*> ([(2*),(3*),(4*)] <*> [1,2,3])
              print $ pairs [1,2,3] ['a','b','c']
              print $ afun P (Just 3) (Just 'a')
              print $ const 5 "jkjer"
              print $ expr1
              print $ expr2 <*> expr1
              print $ expr1
              print $ expr1 >>= h
              print $ fst $ app (alabel tree) 1

sumOfSquares :: Int
sumOfSquares = sum [x*x| x <-[1..100]]

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(a,b) | a <-[0..m], b <-[0..n]]

square :: Int -> [(Int, Int)]
square n = filter(\x -> fst x /= snd x) $ grid n n

replicate' :: Int -> a -> [a]
replicate' n a = [a | _ <-[1..n]]
--replicate' n a = map(const a) [1..n]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z)| x <-[1..n],
                     y <-[1..n],
                     z <-[1..n],
                     x*x+y*y==z*z]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x| x <-[1..n], sum (factors x) == 2*x]

theList :: [(Int,Int)]
theList = concat [[(x,y) | y <- [3,4]] | x <- [1,3]]
theList2 :: [(Int,Int)]
theList2 = [(x,y) | x <- [1,2], y <- [3,4]]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k==k']

positions2 :: Eq a => a -> [a] -> [Int]
positions2 x xs = find x (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

main:: IO()
main = do
  print sumOfSquares
  print $ grid 1 2
  print $ square 2
  print $ replicate' 3 True
  print $ pyths 10
  print $ perfects 500
  print theList
  print theList2
  print $ positions True [False,True,False,True,False]
  print $ positions2 True [False,True,False,True,False]
  print $ scalarproduct [1,2,3] [4,5,6]

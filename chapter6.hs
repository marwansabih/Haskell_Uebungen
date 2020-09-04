(^´) :: Int -> Int -> Int
a ^´ 0 = 1
a ^´ b = a * (a ^´ (b-1))

euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | a > b = euclid (a-b) b
           | otherwise = euclid a (b-a)

and1 :: [Bool] -> Bool
and1 l@(x:xs) | null l = True
              | x == True = and1 xs
              | x == False = False

concate :: [[a]] -> [a]
concate []     = []
concate (x:xs) = x ++ concate xs

replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x = x : replicate1 (n-1) x

(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 a [] = False
elem1 a (x:xs) | x == a = True
               | x /= a = elem1 a xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) (ys)

halve :: [a] -> ([a],[a])
halve a = (take k a, drop k a)
          where k = length a `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst hvs)) (msort (snd hvs))
           where hvs = halve xs

sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

take' :: Int -> [a] -> [a]
take' 0 xs     = []
take' n []     = []
take' n (x:xs) = x : take' (n-1) xs

select :: [a] -> a
select (x:[]) = x
select (x:xs) = select xs

main = do
  print $ 2 ^´ 3
  print $ euclid 27 6
  print $ concate [[23,4,44],[2,3,4,5,7]]
  print $ replicate 10 1
  print $ [10,22,33] !! 2
  print $ elem1 10 [1,7,10,11]
  print $ elem1 10 [1,2,3,4,5,6,11]
  print $ merge [2,5,6] [1,3,4]
  print $ merge [2,5,8,9] [1,3,4]
  print $ msort [3,1,8,4,5]
  print $ sum' [3,2,1,4,5,6]
  print $ take' 3 [1,2,3,4,5]
  print $ select [2,3,4,0,9,10,15]

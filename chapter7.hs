import           Data.Char

filter_map :: (a -> b) -> (a -> Bool) -> [a] -> [b]
filter_map f g  = (map f) . (filter g)

all' :: (a -> Bool) -> [a] -> Bool
all' f = and . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f = or . map f

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = (x:xs)

--map' f xs = foldr (\x -> ([f x]++)) []


map' f  = foldr (\x xs -> f x : xs) []

mfilter' f = foldr (\x xs -> if f x then x:xs else xs)

dec2int xs = foldl (\x n -> 10 * x + n) 0 xs

curry' :: ((a,b) -> c) -> (a -> b -> c)
--curry' f a b = f (a,b)
curry' f = \x y -> f (x,y)

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
-- uncurry' f (a,b) = f a b
uncurry' f = \(a,b) -> f a b

unfold p h t x | p x = []
               | otherwise = h x: unfold p h t (t x)

chop8 = unfold (== []) (take 8) (drop 8)

mapl f = unfold (== []) (f . head) (drop 1)

iterate'' f = unfold (const False) id f

altMap :: (a->b) -> (a->b) -> [a] -> [b]
altMap f g =  (map (\(f,x) -> f x)) . zip ( concat ( repeat [f,g]) )

t = altMap (+10) (+100) [0,1,2,3,4]

luhn x =  let f =  sum . (altMap (1*) (\x -> mod (2*x) 9)) . reverse
          in mod (f x) 10 == 0


type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

addParity :: [Bit] -> [Bit]
addParity xs = xs ++ [mod (sum xs) 2]

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)


make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 = unfold (== []) (take 9) (drop 9)

processParity :: [Bit] -> [Bit]
processParity xs = if sum (init xs) `mod` 2 == last xs
                   then init xs
                   else error "Wrong parity found!"

decode :: [Bit] -> String
decode = map (chr . bin2int . processParity) . chop9

transmit :: String -> String
transmit = decode . channel . encode


channel :: [Bit] -> [Bit]
channel = id

brokenChannel :: [Bit] -> [Bit]
brokenChannel = tail

transmit2 :: String -> String
transmit2 = decode . brokenChannel . encode

main :: IO()
main = do
  putStrLn "Chapter 7"
  print $ filter_map (*2) (/= 2) [1,2,3,4]
  print $ all' (\x -> mod x 2 == 0 ) [2,4,6,8]
  print $ all' (\x -> mod x 2 == 0 ) [2,4,5,6,8]
  print $ any' (\x -> mod x 2 == 0 ) [2,4,5,6,8]
  print $ takeWhile' (\x -> mod x 2 == 0 ) [2,4,6,7,8]
  print $ dropWhile' (\x -> mod x 2 == 0 ) [2,4,5,6,8]
  print $ map' (2*) [1,2,3]
  print $ dec2int [2,3,4,5,6]
  print $ curry' (\(a,b)-> a + b) 1 4
  print $ uncurry' (\x y -> x + y) (4,4)
  print $ chop8 [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]
  print $ mapl (2*) [2,2,3,4,5]
  print $ take 8 $ iterate'' (2*) 1
  print $ t
  print $ luhn [4,7,8,3]
  print $ luhn [1,7,8,4]
  print $ luhn [5,2,2,0,0,1,4,8,8,6]
  print $ transmit "It works!"
  print $ (processParity . addParity) [1,0,1,1]
  print $ transmit2 "It works!"

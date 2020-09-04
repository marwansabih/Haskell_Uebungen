import           Data.Char

let2int :: Char -> Char -> Int
let2int c s = ord c - ord s

int2let :: Int -> Char -> Char
int2let n s = chr (ord s + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c 'a' + n) `mod` 26) 'a'
          | isUpper c = int2let ((let2int c 'A' + n) `mod` 26) 'A'
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n/fromIntegral m) * 100

count x xs = length [x' | x' <-xs, x == x']

lowers xs = length [x | x <- xs, x >='a', x <= 'z']

uppers xs = length [x | x <- xs, x >='A', x <= 'Z']


freqs :: String -> [Float]
freqs xs = [percent (count x xs + count y xs) n | (x,y) <- zip ['a'..'z'] ['A'..'Z']]
        where n = lowers xs + uppers xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i| (x',i) <- zip xs [0..], x == x']

crack :: String -> String
crack xs = encode (-factor) xs
        where
                factor = head (positions (minimum chitab) chitab)
                chitab = [chisqr (rotate n table') table | n <- [0..25]]
                table' = freqs xs

main :: IO()
main = print "Hi"

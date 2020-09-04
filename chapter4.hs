
halve :: [a] -> ([a],[a])
halve x = let l = div (length x) 2
          in ( take l x, drop l x)


third xs = head $ tail $ tail xs

third' xs = xs !! 2

third'' (_:_:a:_) = a

own_and x y =
  if x
    then
      if x
        then True
        else False
  else False

own_and' x y =
  if x
    then y
    else False

mult :: Int -> Int -> Int -> Int
mult = (\x -> (\y -> (\z -> x*y*z)))

luhnDouble x = let n = 2*x
               in if n > 9
                 then n - 9
                 else n

luhn a b c d = let n1 = luhnDouble a
                   n3 = luhnDouble c
                in mod (n1 + b + n3 + d) 10 == 0

own_or True True   = True
own_or True False  = True
own_or False True  = True
own_or False False = False

own_or' False False = False
own_or' _ _         = True

own_or'' True _ = True
own_or'' _ True = True
own_or'' _ _    = False

own_or''' False b = b
own_or''' _ _     = True

safe_tail x = if null x then x else tail x

safe_tail' :: [a] -> [a]
safe_tail' x | null x = x
safe_tail' x | otherwise = tail x

safe_tail'' []     = []
safe_tail'' (x:xs) = xs

main :: IO()
main = do
  print $ halve [1,2,3,4,5,6]
  print $ halve [1]
  print $ third [2,3,4]
  print $ third' [2,3,4]
  print $ third'' [2,3,4]
  print $ luhnDouble 3
  print $ luhnDouble 6
  print $ luhn 1 7 8 4
  print $ luhn 4 7 8 3
  print $ luhn 1 2 3 4

import           System.IO

putStr' :: String -> IO ()
putStr' xs = sequence_ [putChar x | x <-xs]

putRow :: Int -> Int -> IO()
putRow row num = do putStr (show row)
                    putStr (": ")
                    putStrLn (concat (replicate num "* "))




type Board = [Int]
putBoard' :: Int -> Board -> IO ()
putBoard' _ [] = return ()
putBoard' r (x:xs) = do
                    putRow r x
                    putBoard' (r+1) xs

putBoard :: Board -> IO ()
putBoard = putBoard' 1

putBoard'' :: Board -> IO ()
putBoard'' board = let num_board = zip [1..] board
                   in sequence_ $ map ( \x -> putRow (fst x) (snd x)) num_board


readNumbers :: Int -> Int -> IO ()
readNumbers total 0 = do
                     putStrLn $ "The total is " ++ show total
                     return ()
readNumbers total r = do
                      n <- getLine
                      let y = read n :: Int
                      readNumbers (total + y) (r-1)

adder = do
    putStr "How many numbers? "
    x <- getLine
    let y = read x :: Int
    readNumbers 0 y

readNum :: IO Int
readNum = do
        x <- getLine
        let y = read x :: Int
        return y

adder' :: IO ()
adder' = do
  putStr "How many numbers? "
  x <- getLine
  let n = read x :: Int
  let xs = sequence $ replicate n (readNum)
  ns <- xs
  putStrLn $ "The total is " ++ show (sum ns)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n' then
                do putChar x
                   return []
              else
                do putChar '-'
                   xs <- sgetLine
                   return (x:xs)

readLine :: IO String
readLine = do
            x <- getCh
            case x of
              '\n' -> do
                     putChar x
                     return []
              '\DEL' -> do putChar '\b'
                           xs <- readLine
                           return (init xs)
              otherwise -> do putChar x
                              xs <- readLine
                              return (x:xs)

putBoard''' board = sequence_ [putRow row num | (row,num) <- zip [1..] board]
main:: IO()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Hallo"
  putBoard [1,2,3,4,5]
  putBoard'' [1,2,3,4,5]
  putBoard''' [1,2,3,4,5]
  x <- readLine
  putStrLn x

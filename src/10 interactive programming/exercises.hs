import Data.Function

promptForStr :: IO ()
promptForStr = do   putStr "enter a word: "
                    word <- getLine
                    putStr "your input has "
                    word & length & show & putStr
                    putStrLn " characters"
                    

-- 1
-- Redefine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ()
putStr2 :: String -> IO ()
putStr2 str = sequence_ [putChar c | c <- str]

-- 2
-- Using recursion, define a version of putBoard :: Board -> IO () that dis-
-- plays nim boards of any size, rather than being specific to boards with just
-- five rows of stars. Hint: first define an auxiliary function that takes the current
-- row number as an additional argument.
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

putBoardHelper :: Board -> Int -> IO ()
putBoardHelper (x:xs) n | null xs   = do putRow n x
                        | otherwise = do putRow n x
                                         putBoardHelper xs (n+1)

anotherPutBoard :: Board -> IO ()
anotherPutBoard xs = putBoardHelper xs 1

-- 3 
-- n a similar manner to the first exercise, redefine the generalised version of
-- putBoard using a list comprehension and sequence_
seqPutBoard :: Board -> IO ()
seqPutBoard xs = sequence_ [putRow n x | (n, x) <- zip [1..] xs]

-- 4
-- Define an action adder :: IO () that reads a given number of integers from
-- the keyboard, one per line, and displays their sum
-- Hint: start by defining an auxiliary function that takes the current total and
-- how many numbers remain to be read as arguments. You will also likely need
-- to use the library functions read and show.

adderHelper :: Int -> Int -> IO ()
adderHelper currentTotal numbersLeft | numbersLeft == 0 = do putStr "total is: "
                                                             putStrLn (show currentTotal)
                                     | otherwise        = do putStr "give number: "
                                                             input <- getLine
                                                             let x :: Int = read input
                                                             adderHelper (currentTotal + x) (numbersLeft - 1)

adder :: IO ()
adder = do putStr "how many numbers ? "
           input <- getLine
           let x = (read input :: Int)
           adderHelper 0 x

-- 5
-- Redefine adder using the function sequence :: [IO a] -> IO [a] that per-
-- forms a list of actions and returns a list of the resulting values

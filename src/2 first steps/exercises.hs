import Data.Function

-- 3
res :: Int = a `div` length xs
    where
        a = 10
        xs :: [Int] = [1,2,3,4,5]
        

-- 4
last2 :: [a] -> a
last2 xs = xs & drop n & head
    where n :: Int = length xs - 1

-- 5
init2 :: [a] -> [a]
init2 xs = take n xs
    where n = length xs - 1

init3 :: [a] -> [a]
init3 xs = xs & reverse & tail & reverse

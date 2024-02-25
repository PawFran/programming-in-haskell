-- 1
l1 :: String = ['a', 'b']
l2 :: (Char, Char) = ('a', 'b')
l3 :: [(Bool, Char)] = [(False, '0'), (True, '1')]
l4 :: ([Bool], String) = ([False, True], ['0', '1'])
--  l5 :: [[a] -> [a]] but somehow there's error durin import in ghci
l5 = [tail, init, reverse]


-- 2
bools :: [Bool] = [True, False]

nums :: [[Int]] = [[1, 2], [1..10]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3
second :: [a] -> a 
second xs = head $ tail xs

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f $ f x

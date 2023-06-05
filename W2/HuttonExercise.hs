--- Exercises First Steps
-- Exercise 3
myLast x = head (reverse x)

-- Exercise 4
myLast2 x = x !! ((length x)-1)

-- Exercise 5
myInit x = reverse (drop 1 (reverse x))
myInit2 x = take ((length x)-1) x


--- Exercises Types and Classes

-- Exercise 1
-- :type ['a','b','c']
-- :type ('a','b','c')
-- :type [(False,'0'),(True,'1')]
-- :type ([False,True],['0','1'])

-- Exercise 2
second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

double :: Num a => a -> a
double x = 2*x

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (a -> a) -> a -> a
twice f x = f (f x)
import Prelude hiding ((.), ($)) --this hides this two functions

(.) :: (b -> c) -> (a -> b) -> (a -> c)
g . f = \x -> g (f x)

test1 = (head . tail) [1,2,3,4] == head (tail [1,2,3,4])

(.>) :: (a -> b) -> (b -> c) -> (a -> c)
(.>) = flip (.)

test2 = (tail .> head) [1,2,3] == head (tail [1,2,3])

($) :: (a -> b) -> a -> b
f $ x = f x

-- f $ g $ x instead of f (g x)
test3 = head $ tail [1,2,3,4] == head (tail [1,2,3,4])
-- Explanation: ($5) is a function which we still can input a function f. and we map these to the function (+1) and (*2)
test4 = map ($5) [(+1), (*2)] == [6, 10]

($>) :: a -> (a -> b) -> b
($>) = flip $








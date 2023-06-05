module Exer04Prob where

-- Exercise (Higher-Order Functions)

import Prelude hiding (flip, curry, uncurry)

toBeImplemented = undefined

-- flip f takes its (first) two arguments in the reverse order of f
exaFlip = flip take [1,2,3,4,5] 3 == [1,2,3]

flip :: (a -> b -> c) -> (b -> a -> c)
flip = toBeImplemented

-- curry converts a function on pairs to a curried function
exaCurry = curry (\(x,y) -> x + y) 3 4 == 7

curry :: ((a, b) -> c) -> (a -> b -> c)
curry = toBeImplemented

-- uncurry converts a curried function to a function on pairs
exaUncurry = uncurry (\x y -> x + y) (3, 4) == 7

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry = toBeImplemented

exaReverseFr = reverseFr [1 .. 20000] == [20000, 19999 .. 1]
exaReverseFl = reverseFl [1 .. 20000] == [20000, 19999 .. 1]

-- implement reverse using foldr and (++)
reverseFr :: [a] -> [a]
reverseFr = toBeImplemented

-- implement reverse using foldl, (:), and flip
reverseFl :: [a] -> [a]
reverseFl = toBeImplemented

-- revAppend prepends the first list in reverse order before the second list
exaRevAppend = revAppend [3,2,1] [4,5,6] == [1,2,3,4,5,6]
-- implement revAppend using foldl and flip

revAppend :: [a] -> [a] -> [a]
revAppend = toBeImplemented

exaMapFr = mapFr (*2) [1 .. 10] == map (*2) [1 .. 10]
exaMapFl = mapFl (*2) [1 .. 10] == map (*2) [1 .. 10]

mapFr :: (a -> b) -> [a] -> [b]
mapFr = toBeImplemented

mapFl :: (a -> b) -> [a] -> [b]
mapFl = toBeImplemented

-- for estimating performance
lr = length (mapFr (*2) [1 .. 20000])
ll = length (mapFl (*2) [1 .. 20000])

exaFilterFr = filterFr odd [1 .. 10] == filterFr odd [1 .. 10]
exaFilterFl = filterFl odd [1 .. 10] == filterFl odd [1 .. 10]

filterFr :: (a -> Bool) -> [a] -> [a]
filterFr = toBeImplemented

filterFl :: (a -> Bool) -> [a] -> [a]
filterFl = toBeImplemented

exaLengthFr = lengthFr [1 .. 1000] == 1000
exaLengthFl = lengthFl [1 .. 1000] == 1000

lengthFr :: [a] -> Int
lengthFr = toBeImplemented

lengthFl :: [a] -> Int
lengthFl = toBeImplemented

exaAppendFr = appendFr [4,5,6] [1,2,3] == [1 .. 6]
exaAppendFl = appendFl [1,2,3] [4,5,6] == [1 .. 6]

appendFr :: [a] -> [a] -> [a]
appendFr = toBeImplemented

appendFl :: [a] -> [a] -> [a]
appendFl = toBeImplemented

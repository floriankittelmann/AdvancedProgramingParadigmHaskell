-- Exercise 8 (Test Framework and Permutations)

toBeImplemented = undefined

-- general test framework
-- first parameter of function test should be removed
-- as soon as we have a set type as instance of class Eq
-- at our disposal

test :: (f -> (a -> b)) -> (b -> b -> Bool) -> f -> [(a, b)] -> Bool
test = toBeImplemented

-- function setEq
-- setEq compares two sets represented by lists for equality
-- both lists are permitted to contain duplicates

setEq :: Eq a => [a] -> [a] -> Bool
setEq xs ys = all (`elem` ys) xs && all (`elem` xs) ys

check1SetEq = setEq [1,2,3,2] [2,3,1,1]
check2SetEq = not $ setEq [1,2,3] [2,3,4]

uncurry3 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (x, y, z) = f x y z

test3 ::
  (b -> b -> Bool) -> (a1 -> a2 -> a3 -> b) -> [((a1, a2, a3), b)] -> Bool
test3 = toBeImplemented

-- function revApp
-- revApp rs xs puts the reverse of rs in front of xs

revApp :: [a] -> [a] -> [a]
revApp = flip (foldl (flip (:)))

checkRevApp = revApp [3,2,1] [4,5,6] == [1,2,3,4,5,6]

-- function insert
-- insert x ys n inserts x into list ys at position n
-- precondition: 0 <= n <= length ys

type Insert a = a -> [a] -> Int -> [a]

insertTests =
  [((99, [1,2,3], 0), [99,1,2,3]),
   ((99, [1,2,3], 1), [1,99,2,3]),
   ((99, [1,2,3], 2), [1,2,99,3]),
   ((99, [1,2,3], 3), [1,2,3,99])]

insertV1 :: Insert a
insertV1 = toBeImplemented

insertV2 :: Insert a
insertV2 = toBeImplemented

insertV1Test = test3 (==) insertV1 insertTests
insertV2Test = test3 (==) insertV2 insertTests

insert :: Insert a
insert = insertV1

-- function inserts
-- inserts x ys inserts x at all possible positions in ys
-- the order of the insertions remains unspecified

type Inserts a = a -> [a] -> [[a]]

-- actually just one test
insertsTests =
  [((99, [1,2,3]), [[99,1,2,3],
                    [1,99,2,3],
                    [1,2,99,3],
                    [1,2,3,99]])]

insertsV1 :: Inserts a
insertsV1 = toBeImplemented

insertsV2 :: Inserts a
insertsV2 = toBeImplemented

insertsV1Test = test uncurry setEq insertsV1 insertsTests
insertsV2Test = test uncurry setEq insertsV2 insertsTests

inserts :: Inserts a
inserts = insertsV1

-- function outsert
-- outsert xs n yields the element at position n and
-- list xs shortened by that element
-- precondition: 0 <= n < length xs

outsertTests =
  [(([1,2,3], 0), (1, [2,3])),
   (([1,2,3], 1), (2, [1,3])),
   (([1,2,3], 2), (3, [1,2]))]

outsert :: [a] -> Int -> (a, [a])
outsert = toBeImplemented

outsertTest = test uncurry (==) outsert outsertTests

-- function perms
-- perms xs yields the list of all permutations of xs
-- the order of the permutations remains unspecified
-- precondition: all elements in xs are pairwise distinct

permsTests =
  [([],      [[]]),
   ([1],     [[1]]),
   ([1,2],   [[1,2], [2,1]]),
   ([1,2,3], [[1,2,3], [1,3,2],
              [2,1,3], [2,3,1],
              [3,1,2], [3,2,1]])]

permsV1 :: [a] -> [[a]]
permsV1 = toBeImplemented

permsV2 :: [a] -> [[a]]
permsV2 = toBeImplemented

permsV3 :: [a] -> [[a]]
permsV3 = toBeImplemented

permsV4 :: [a] -> [[a]]
permsV4 = toBeImplemented

permsV5 :: [a] -> [[a]]
permsV5 = toBeImplemented

permsV6 :: Eq a => [a] -> [[a]]
permsV6 = toBeImplemented

permsTest =
  all (\perms -> test id setEq perms permsTests)
    [permsV1, permsV2, permsV3, permsV4, permsV5, permsV6]

perms :: [a] -> [[a]]
perms = permsV1

-- an application: concurrent programming
-- determine the results of all possible schedulings
-- of some atomic computations

atomA1 x = x + 1
atomA2 x = 2 * x
atomA3 x = x * x

atomB1 x = 2*x + 1
atomB2 x = 3*x + 2
atomB3 x = 4*x + 3

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

allResultsA x = map (($x).compose) (perms [atomA1, atomA2, atomA3])
allResultsB x = map (($x).compose) (perms [atomB1, atomB2, atomB3])

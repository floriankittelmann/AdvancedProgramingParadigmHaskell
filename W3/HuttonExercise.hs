andH :: [Bool] -> Bool
andH [] = True
andH (x:xs) = x && andH xs

concatH :: [[a]] -> [a]
concatH [] = []
concatH (x:xs) = x ++ concatH xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : (replicate' (n-1) x)

nElement :: [a] -> Int -> a
nElement (x:xs) 0 = x
nElement (x:xs) n = nElement xs (n-1)


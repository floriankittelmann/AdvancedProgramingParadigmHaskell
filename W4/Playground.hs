rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

rev2 :: [a] -> [a]
rev2 xs = h xs []
  where
    h :: [s] -> [s] -> [s]
    h [] accu = accu
    h (x:xs) accu = h xs (x:accu)
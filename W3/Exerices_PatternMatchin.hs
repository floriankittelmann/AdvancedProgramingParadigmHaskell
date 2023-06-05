-- Last Exercise
g1 "dimdi" = 1
g1 ['d', 'o', 'm', 'd', 'o'] = 2
g1 ('d' : 'i' : 'n' : 'g' : []) = 3
g1 ('d' : 'i' : 'n' : 'g' : _) = 4
g1 (x : y) = 5
g1 _ = 6

g2 (d : "imdi") | d == 'd' || d == 'D' = 1
g2 (z : "umsel") | z == 'z' || z == 'Z' = 2
g2 _ = 3

h3 ((x : y) : z) = y
h3 ([] : _) = "2"
h3 [] = "3"
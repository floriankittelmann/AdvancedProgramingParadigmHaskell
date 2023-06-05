-- Example 2
data Shape = Circle Double | Rect Double Double deriving (Show, Eq)

area :: Shape -> Shape
area (Circle r) = pi * (r*r)
area (Rect x y) = x * y
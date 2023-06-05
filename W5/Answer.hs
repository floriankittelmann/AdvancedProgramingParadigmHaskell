-- Example 1
data Answer = Yes | No | Unknown deriving (Show, Eq)

flip :: Answer -> Answer
flip Yes = No
flip No = Yes
flip Unknown = Unknown
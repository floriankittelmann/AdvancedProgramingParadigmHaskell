--- hand made definition of a List
data List a = Nil | Cons a (List a) deriving Show

exaList1 = Cons 3 (Cons 5 Nil)
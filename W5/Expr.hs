-- Example 4
data Op = Add | Mult | Sub deriving Show

data Expr
  = Lit Int
  | Dyadic Op Expr Expr
  deriving Show

-- 2 * (1+5)
ex1 = Dyadic Mult (Lit 2) (Dyadic Add (Lit 1) (Lit 5))

evalOp :: Op -> Int -> Int -> Int
evalOp Add = (+)
evalOp Mult = (*)
evalOp Sub = (-)

eval :: Expr -> Int
eval (Lit n) = n
eval (Dyadic op e1 e2) = eval e1 `f` eval e2
  where f = evalOp op


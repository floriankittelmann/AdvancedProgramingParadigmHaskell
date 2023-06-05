-- copied from W5 Expr.hs
data Op = Add | Mult | Sub deriving Show

type Value = Integer
type Ident = String

type State = Ident -> Value

exaState1 :: Ident -> Value
exaState1 "q"
  | 1 == 1 = 17
  | False = 38
exaState1 "dimdi" = 42
exaState1 _ = 0

readState :: State -> Ident -> Value
readState state ident = state ident

updateState :: State -> (Ident, Value) -> State
updateState state (ident, value) = \ident' -> if ident' == ident then value else state ident'

exaState2 = updateState exaState1 ("dimdi", 84)

data Expr
  = Lit Value
  | IdAExpr Ident
  | Dyadic Op Expr Expr
  deriving Show

-- 2 * (1+5)
ex1 = Dyadic Mult (Lit 2) (Dyadic Add (Lit 1) (Lit 5))

-- q + 1
exaAExpr2 = Dyadic Add (IdAExpr "q") (Lit 1)

evalOp :: Op -> Value -> Value -> Value
evalOp Add = (+)
evalOp Mult = (*)
evalOp Sub = (-)

eval :: Expr -> State -> Value
eval (Lit n) _ = n
eval (IdAExpr ident) state = readState state ident --- readState is a lookup mechanism
eval (Dyadic op e1 e2) state = eval e1 state `f` eval e2 state
  where f = evalOp op

evalRelOpr GreaterEq ..

evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (RelBExpr opr e1 e2) =
  (evalRelOpr opr (eval e1 state) (eval e2 state)

data BoolExpr
  = RelBExpr RelOpr Expr Expr
  deriving Show

data RelOpr
  = GreaterEq
  | LessEq
  deriving Show

data Command
  = AssiCmd Ident Expr
  | CpsCmd [Command]
  | WhileCmd BoolExpr Command
  deriving Show

divCmd =
  CpsCmd [
    AssiCmd "q" (Lit 0),
    AssiCmd "r" (IdAExpr "a"),
    WhileCmd (RelBExpr GreaterEq (IdAExpr "r") (IdAExpr "b"))
      (CpsCmd [
        AssiCmd "q" (Dyadic Add (IdAExpr "q") (Lit 1)),
        AssiCmd "r" (Dyadic Sub (IdAExpr "r") (IdAExpr "b"))
      ])
  ]

interCmd :: Command -> State -> State
interCmd (AssiCmd ident ae) state =
  updateState state ident (eval ae)
interCmd (CpsCmd cmds) state
  = foldl interCmd cmds state
interCmd (WhileCmd guard rep) state
  = evalBExpr guard state -> interCmd (WhileCmd guard rep) state'
  | otherwise -> state
  where state' = interCmd rep state
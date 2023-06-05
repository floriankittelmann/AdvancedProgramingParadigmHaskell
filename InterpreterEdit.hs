-- An interpreter for a tiny imperative programming language

module InterpreterEdit where

type Value = Integer
type Ident = String

type State = Ident -> Value

readS :: State -> Ident -> Value --- where is the function State defined ?
readS state ident = state ident

updateS :: State -> (Ident, Value) -> State --- would that if statement as well be possible with guards | ?
updateS state (ident, value) =
  \ident' -> if ident' == ident then value
             else state ident'

exaState1 :: State -- is equal to Ident -> Value therfore exaState1 is a function of Type State
exaState1 "q" = 17
exaState1 "dimdi" = 42
exaState1 _ = 0

exaState2 = updateS exaState1 ("dimdi", 84)

data ArithOpr = Add | Mult | Sub
  deriving Show

data ArithExpr
  = Lit Value
  | IdAExpr Ident
  | Dyadic ArithOpr ArithExpr ArithExpr
  deriving Show

-- 2 * (1 + 5)
-- q + 1

exaAExpr1 = Dyadic Mult (Lit 2) (Dyadic Add (Lit 1) (Lit 5))
exaAExpr2 = Dyadic Add (IdAExpr "q") (Lit 1)

evalAOpr :: ArithOpr -> Value -> Value -> Value --- here we implement a curried function, we return a function where I can still input the two values
evalAOpr Add = (+)
evalAOpr Mult = (*)
evalAOpr Sub = (-)

evalAExpr :: ArithExpr -> State -> Value
evalAExpr (Lit n) _ = n
evalAExpr (IdAExpr ident) state = readS state ident
evalAExpr (Dyadic opr e1 e2) state =
    evalAExpr e1 state `opn` evalAExpr e2 state
  where opn = evalAOpr opr

data RelOpr -- I define a new dataType called RelOpr. The possible Values of this datatype is 'GreaterEq' or 'LessEq'
  = GreaterEq
  | LessEq
  deriving Show

evalRelOpr :: RelOpr -> Value -> Value -> Bool
evalRelOpr GreaterEq = (>=)
evalRelOpr LessEq = (<=)

data BoolExpr
  = RelBExpr RelOpr ArithExpr ArithExpr
  deriving Show

evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (RelBExpr opr e1 e2) state =
  evalRelOpr opr (evalAExpr e1 state) (evalAExpr e2 state)

data Command
  = AssiCmd Ident ArithExpr
  | CpsCmd [Command]
  | WhileCmd BoolExpr Command
  deriving Show

interCmd :: Command -> State -> State
interCmd (AssiCmd ident ae) state =
  updateS state (ident, evalAExpr ae state)
interCmd (CpsCmd cmds) state =
  foldl (flip interCmd) state cmds -- why is here a flip necessary? foldl interCmd cmds state is not possible?
interCmd (WhileCmd guard rep) state
  | evalBExpr guard state = interCmd (WhileCmd guard rep) state'
  | otherwise = state
  where state' = interCmd rep state

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

state :: State
state "a" = 100
state "b" = 33

{-
testDiv = interCmd divCmd exaStateDiv


  q := 0;
  r := a;
  while r >= b do
    q := q + 1;
    r := r - b
  endwhile
-}

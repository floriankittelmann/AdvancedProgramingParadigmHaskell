data Type = NatTy | IntTy deriving (Eq, Show)

type Value = Integer
type Ident = String
type TyEnv = Ident -> Type

data Expr
  = Lit Integer
  | Var Ident
  | Add Expr Expr
  | Sub Expr Expr
  | Cast Expr Type
  deriving (Eq, Show)

ty :: TyEnv -> Expr -> Type
ty _ (Lit n)
  | n >= 0 = NatTy
  | otherwise = IntTy
ty env (Var id) = env id
ty env (Add ex1 ex2)
  | ty env ex1 == NatTy && ty env ex2 == NatTy = NatTy
  | otherwise = IntTy
ty _ (Sub ex1 ex2) = IntTy
ty _ (Cast ex typeVar) = typeVar
data Linie = La | Lb | Lc | Ld | Le | Lf | Lg | Lh
  deriving (Eq, Show)

data Reihe = R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8
  deriving (Eq, Show)

type Pos = (Linie, Reihe)

data Farbe = Weiss | Schwarz
  deriving (Eq, Show)

data FigurTyp
  = K -- Koenig
  | D -- Dame
  | T -- Turm
  | L -- Laeufer
  | S -- Springer
  | B -- Bauer
  deriving (Eq, Show)

type Figur = (Farbe, FigurTyp)

type Feld = Maybe Figur

type Brett = [(Pos, Figur)]

brett01 = [((Lc,R6),(Schwarz,S)), ((Le,R1),(Weiss,K))]
pos01   = (Le, R1)
pos02   = (Le, R2)

getFeld2 :: Brett -> Pos -> Feld
getFeld2 = flip lookup
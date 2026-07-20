import Data.List (sortOn)

-- 1. Frações.

data Frac = F Integer Integer

mdc :: Integer -> Integer -> Integer
mdc x 0 = abs x
mdc x y = mdc y (mod x y)

-- Fração irredutível com denominador positivo.
normaliza :: Frac -> Frac
normaliza (F _ 0) = error "Denominador nulo"
normaliza (F 0 _) = F 0 1
normaliza (F x y) = F (sinal * div x g) (abs (div y g))
  where g     = mdc x y
        sinal = if y < 0 then -1 else 1

instance Eq Frac where
  f1 == f2 = n1 == n2 && d1 == d2
    where F n1 d1 = normaliza f1
          F n2 d2 = normaliza f2

instance Ord Frac where
  f1 <= f2 = n1 * d2 <= n2 * d1
    where F n1 d1 = normaliza f1
          F n2 d2 = normaliza f2

instance Show Frac where
  show f | d == 1    = show n
         | otherwise = "(" ++ show n ++ "/" ++ show d ++ ")"
    where F n d = normaliza f

instance Num Frac where
  (F n1 d1) + (F n2 d2) = normaliza (F (n1*d2 + n2*d1) (d1*d2))
  (F n1 d1) - (F n2 d2) = normaliza (F (n1*d2 - n2*d1) (d1*d2))
  (F n1 d1) * (F n2 d2) = normaliza (F (n1*n2) (d1*d2))
  negate (F n d) = F (-n) d
  abs (F n d)    = F (abs n) (abs d)
  signum f = F (signum n) 1
    where F n _ = normaliza f
  fromInteger x = F x 1

-- Elementos de l maiores do que o dobro de f.
maioresDobro :: Frac -> [Frac] -> [Frac]
maioresDobro f = filter (> 2*f)

-- 2. Expressões genéricas.

data Exp a = Const a
           | Simetrico (Exp a)
           | Mais (Exp a) (Exp a)
           | Menos (Exp a) (Exp a)
           | Mult (Exp a) (Exp a)

instance Show a => Show (Exp a) where
  show (Const a)     = show a
  show (Simetrico e) = "(-" ++ show e ++ ")"
  show (Mais e1 e2)  = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
  show (Menos e1 e2) = "(" ++ show e1 ++ "-" ++ show e2 ++ ")"
  show (Mult e1 e2)  = "(" ++ show e1 ++ "*" ++ show e2 ++ ")"

instance Eq a => Eq (Exp a) where
  (Const x)     == (Const y)     = x == y
  (Simetrico x) == (Simetrico y) = x == y
  (Mais x y)    == (Mais w z)    = x == w && y == z
  (Menos x y)   == (Menos w z)   = x == w && y == z
  (Mult x y)    == (Mult w z)    = x == w && y == z
  _             == _             = False

instance Num a => Num (Exp a) where
  fromInteger n = Const (fromInteger n)
  x + y = Mais x y
  x - y = Menos x y
  x * y = Mult x y
  negate = Simetrico
  abs = id
  signum _ = Const (fromInteger 0)

-- 3. Extrato bancário.

data Movimento = Credito Float | Debito Float
data Data = D Int Int Int
data Extracto = Ext Float [(Data, String, Movimento)]

instance Eq Data where
  D a1 m1 d1 == D a2 m2 d2 = (a1,m1,d1) == (a2,m2,d2)

instance Ord Data where
  D a1 m1 d1 <= D a2 m2 d2 = (a1,m1,d1) <= (a2,m2,d2)

instance Show Data where
  show (D a m d) = show a ++ "/" ++ show m ++ "/" ++ show d

-- Ordena os movimentos por data.
ordena :: Extracto -> Extracto
ordena (Ext saldo l) = Ext saldo (sortOn (\(d,_,_) -> d) l)

saldoFinal :: Float -> [(Data, String, Movimento)] -> Float
saldoFinal s []                    = s
saldoFinal s ((_,_,Credito x):t)   = saldoFinal (s+x) t
saldoFinal s ((_,_,Debito x):t)    = saldoFinal (s-x) t

instance Show Extracto where
  show ext@(Ext saldoIni l) =
    "Saldo anterior: " ++ show saldoIni ++ "\n" ++
    replicate 40 '-' ++ "\n" ++
    "Data        Descricao   Credito   Debito\n" ++
    replicate 40 '-' ++ "\n" ++
    concatMap linha lOrd ++
    replicate 40 '-' ++ "\n" ++
    "Saldo actual: " ++ show (saldoFinal saldoIni l)
    where Ext _ lOrd = ordena ext
          linha (dt, desc, mov) =
            pad 12 (show dt) ++ pad 12 desc ++ valor mov ++ "\n"
          valor (Credito x) = show x
          valor (Debito x)  = replicate 10 ' ' ++ show x
          pad n s = s ++ replicate (max 1 (n - length s)) ' '

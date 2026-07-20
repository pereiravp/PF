data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show

-- 1. Árvores binárias.

altura :: BTree a -> Int
altura Empty        = 0
altura (Node _ e d) = 1 + max (altura e) (altura d)

contaNodos :: BTree a -> Int
contaNodos Empty        = 0
contaNodos (Node _ e d) = 1 + contaNodos e + contaNodos d

folhas :: BTree a -> Int
folhas Empty              = 0
folhas (Node _ Empty Empty) = 1
folhas (Node _ e d)       = folhas e + folhas d

-- Remove os nodos a partir de uma dada profundidade.
prune :: Int -> BTree a -> BTree a
prune _ Empty        = Empty
prune 0 _            = Empty
prune n (Node r e d) = Node r (prune (n-1) e) (prune (n-1) d)

-- Nodos ao longo de um caminho (False = esquerda, True = direita).
path :: [Bool] -> BTree a -> [a]
path _ Empty            = []
path [] (Node r _ _)    = [r]
path (b:bs) (Node r e d)
  | b         = r : path bs d
  | otherwise = r : path bs e

mirror :: BTree a -> BTree a
mirror Empty        = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)

zipWithBT :: (a -> b -> c) -> BTree a -> BTree b -> BTree c
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)
zipWithBT _ _ _                             = Empty

unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (a,b,c) e d) = (Node a ea da, Node b eb db, Node c ec dc)
  where (ea, eb, ec) = unzipBT e
        (da, db, dc) = unzipBT d

-- 2. Árvores binárias de procura.

minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node _ e _)     = minimo e

semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node _ Empty d) = d
semMinimo (Node r e d)     = Node r (semMinimo e) d

-- Mínimo e árvore sem mínimo numa só travessia.
minSmin :: Ord a => BTree a -> (a, BTree a)
minSmin (Node r Empty d) = (r, d)
minSmin (Node r e d)     = (m, Node r e' d)
  where (m, e') = minSmin e

remove :: Ord a => a -> BTree a -> BTree a
remove _ Empty = Empty
remove x (Node r e d)
  | x < r     = Node r (remove x e) d
  | x > r     = Node r e (remove x d)
  | otherwise = case d of
      Empty -> e
      _     -> let (m, d') = minSmin d in Node m e d'

-- 3. Turma (árvore de procura ordenada por número).

type Aluno = (Numero, Nome, Regime, Classificacao)
type Numero = Int
type Nome   = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int | Rep | Faltou deriving Show
type Turma = BTree Aluno

inscNum :: Numero -> Turma -> Bool
inscNum _ Empty = False
inscNum n (Node (a,_,_,_) e d)
  | n == a    = True
  | n > a     = inscNum n d
  | otherwise = inscNum n e

inscNome :: Nome -> Turma -> Bool
inscNome _ Empty = False
inscNome nm (Node (_,a,_,_) e d) = nm == a || inscNome nm e || inscNome nm d

-- Trabalhadores-estudantes, ordenados por número.
trabEst :: Turma -> [(Numero, Nome)]
trabEst Empty = []
trabEst (Node (num,nm,TE,_) e d) = trabEst e ++ [(num,nm)] ++ trabEst d
trabEst (Node _ e d)             = trabEst e ++ trabEst d

nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (a,_,_,c) e d)
  | n == a    = Just c
  | n < a     = nota n e
  | otherwise = nota n d

contaFaltas :: Turma -> Int
contaFaltas Empty = 0
contaFaltas (Node (_,_,_,Faltou) e d) = 1 + contaFaltas e + contaFaltas d
contaFaltas (Node _ e d)              = contaFaltas e + contaFaltas d

percFaltas :: Turma -> Float
percFaltas Empty = 0
percFaltas t     = fromIntegral (contaFaltas t) / fromIntegral (contaNodos t) * 100

-- Média das notas dos alunos aprovados.
mediaAprov :: Turma -> Float
mediaAprov t | n == 0    = 0
             | otherwise = fromIntegral s / fromIntegral n
  where (s, n) = aux t
        aux Empty = (0, 0)
        aux (Node (_,_,_,Aprov x) e d) = (x + se + sd, 1 + ne + nd)
          where (se, ne) = aux e
                (sd, nd) = aux d
        aux (Node _ e d) = (se + sd, ne + nd)
          where (se, ne) = aux e
                (sd, nd) = aux d

-- Rácio aprovados/avaliados numa só travessia.
aprovAv :: Turma -> Float
aprovAv t | aval == 0 = 0
          | otherwise = aprov / aval
  where (aprov, aval) = aux t
        aux Empty = (0, 0)
        aux (Node (_,_,_,c) e d) =
          (contaA c + ae + ad, contaAv c + ave + avd)
          where (ae, ave) = aux e
                (ad, avd) = aux d
        contaA (Aprov _) = 1
        contaA _         = 0
        contaAv Faltou = 0
        contaAv _      = 1

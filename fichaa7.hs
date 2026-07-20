-- 1. Expressões inteiras.

data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

calcula :: ExpInt -> Int
calcula (Const x)     = x
calcula (Simetrico e) = - calcula e
calcula (Mais e1 e2)  = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (Mult e1 e2)  = calcula e1 * calcula e2

infixa :: ExpInt -> String
infixa (Const x)     = show x
infixa (Simetrico e) = "-(" ++ infixa e ++ ")"
infixa (Mais e1 e2)  = "(" ++ infixa e1 ++ "+" ++ infixa e2 ++ ")"
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ "-" ++ infixa e2 ++ ")"
infixa (Mult e1 e2)  = "(" ++ infixa e1 ++ "*" ++ infixa e2 ++ ")"

posfixa :: ExpInt -> String
posfixa (Const x)     = show x
posfixa (Simetrico e) = posfixa e ++ " -"
posfixa (Mais e1 e2)  = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult e1 e2)  = posfixa e1 ++ " " ++ posfixa e2 ++ " *"

-- 2. Árvores com número arbitrário de filhos.

data RTree a = R a [RTree a]

soma :: Num a => RTree a -> a
soma (R x fs) = x + sum (map soma fs)

altura :: RTree a -> Int
altura (R _ []) = 1
altura (R _ fs) = 1 + maximum (map altura fs)

prune :: Int -> RTree a -> RTree a
prune 0 (R x _)  = R x []
prune n (R x fs) = R x (map (prune (n-1)) fs)

mirror :: RTree a -> RTree a
mirror (R x fs) = R x (reverse (map mirror fs))

postorder :: RTree a -> [a]
postorder (R x fs) = concatMap postorder fs ++ [x]

-- 3. Árvores de folhas (LTree) e árvores completas (FTree).

data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

ltSum :: Num a => LTree a -> a
ltSum (Tip x)    = x
ltSum (Fork e d) = ltSum e + ltSum d

listaLT :: LTree a -> [a]
listaLT (Tip x)    = [x]
listaLT (Fork e d) = listaLT e ++ listaLT d

ltHeight :: LTree a -> Int
ltHeight (Tip _)    = 1
ltHeight (Fork e d) = 1 + max (ltHeight e) (ltHeight d)

data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

-- Separa a informação dos nodos da informação das folhas.
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf b)  = (Empty, Tip b)
splitFTree (No a e d) = (Node a be bd, Fork le ld)
  where (be, le) = splitFTree e
        (bd, ld) = splitFTree d

-- Reconstrói a FTree, se as formas das duas árvores forem compatíveis.
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees Empty (Tip b) = Just (Leaf b)
joinTrees (Node a be bd) (Fork le ld) =
  case (joinTrees be le, joinTrees bd ld) of
    (Just e, Just d) -> Just (No a e d)
    _                -> Nothing
joinTrees _ _ = Nothing

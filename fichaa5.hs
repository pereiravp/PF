import Data.List (sortOn, groupBy)

-- 1. Reimplementação de funções de ordem superior.

myany :: (a -> Bool) -> [a] -> Bool
myany _ []    = False
myany p (h:t) = p h || myany p t

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f (x:xs) (y:ys) = f x y : myZipWith f xs ys
myZipWith _ _ _           = []

mytakeWhile :: (a -> Bool) -> [a] -> [a]
mytakeWhile _ [] = []
mytakeWhile p (h:t) | p h       = h : mytakeWhile p t
                    | otherwise = []

mydropWhile :: (a -> Bool) -> [a] -> [a]
mydropWhile _ [] = []
mydropWhile p (h:t) | p h       = mydropWhile p t
                    | otherwise = h:t

-- span sem trabalho redundante.
myspan :: (a -> Bool) -> [a] -> ([a], [a])
myspan _ [] = ([], [])
myspan p (h:t)
  | p h       = (h:ini, resto)
  | otherwise = ([], h:t)
  where (ini, resto) = myspan p t

mydeleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
mydeleteBy _ _ [] = []
mydeleteBy eq x (h:t)
  | eq x h    = t
  | otherwise = h : mydeleteBy eq x t

-- Quicksort segundo uma chave.
mysortOn :: Ord b => (a -> b) -> [a] -> [a]
mysortOn _ [] = []
mysortOn f (x:xs) = mysortOn f menores ++ [x] ++ mysortOn f maiores
  where menores = [y | y <- xs, f y <  f x]
        maiores = [y | y <- xs, f y >= f x]

-- 2. Polinómios com funções de ordem superior.

type Polinomio = [Monomio]
type Monomio   = (Float, Int)

selgrau :: Int -> Polinomio -> Polinomio
selgrau n = filter ((== n) . snd)

conta :: Int -> Polinomio -> Int
conta n = length . selgrau n

grau :: Polinomio -> Int
grau [] = 0
grau p  = maximum (map snd p)

deriv :: Polinomio -> Polinomio
deriv = map (\(c,e) -> (c * fromIntegral e, e-1)) . filter ((> 0) . snd)

calcula :: Float -> Polinomio -> Float
calcula x = sum . map (\(c,e) -> c * x^e)

simp :: Polinomio -> Polinomio
simp = filter ((/= 0) . fst)

mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) = map (\(c',e') -> (c*c', e+e'))

ordena :: Polinomio -> Polinomio
ordena = sortOn snd

-- Junta monómios do mesmo grau.
normaliza :: Polinomio -> Polinomio
normaliza = map somaCoefs . groupBy (\x y -> snd x == snd y) . ordena
  where somaCoefs ms = (sum (map fst ms), snd (head ms))

soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza (concatMap (`mult` p2) p1)

equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = normaliza (simp p1) == normaliza (simp p2)

-- 3. Matrizes.

type Mat a = [[a]]

-- Todas as linhas com a mesma dimensão.
dimOK :: Mat a -> Bool
dimOK []      = True
dimOK (h:t)   = all ((== length h) . length) t

dimMat :: Mat a -> (Int, Int)
dimMat []      = (0, 0)
dimMat m@(h:_) = (length m, length h)

addMat :: Num a => Mat a -> Mat a -> Mat a
addMat = zipWith (zipWith (+))

transpose :: Mat a -> Mat a
transpose ([]:_) = []
transpose m      = map head m : transpose (map tail m)

multMat :: Num a => Mat a -> Mat a -> Mat a
multMat m1 m2 = [[sum (zipWith (*) linha coluna) | coluna <- transpose m2] | linha <- m1]

zipWMat :: (a -> b -> c) -> Mat a -> Mat b -> Mat c
zipWMat f = zipWith (zipWith f)

-- Triangular superior: abaixo da diagonal só há zeros.
triSup :: (Num a, Eq a) => Mat a -> Bool
triSup = aux 0
  where aux _ []    = True
        aux n (h:t) = all (== 0) (take n h) && aux (n+1) t

rotateLeft :: Mat a -> Mat a
rotateLeft ([]:_) = []
rotateLeft m      = map last m : rotateLeft (map init m)

import Data.Char (isDigit, isLower, digitToInt)


-- Exercício 1: valores por redução.
-- funA [2,3,5,1] = 39 ; funB [8,5,12] = [8,12] ; funC [1,2,3,4,5] = [5] ; funD "otrec" = "certo"

dobros :: [Float] -> [Float]
dobros = map (2*)

numOcorre :: Char -> String -> Int
numOcorre _ [] = 0
numOcorre x (h:t) | x == h    = 1 + numOcorre x t
                  | otherwise = numOcorre x t

positivos :: [Int] -> Bool
positivos []    = True
positivos (h:t) = h > 0 && positivos t

soPos :: [Int] -> [Int]
soPos = filter (> 0)

somaNeg :: [Int] -> Int
somaNeg = sum . filter (< 0)

tresUlt :: [a] -> [a]
tresUlt l | length l <= 3 = l
          | otherwise     = tresUlt (tail l)

segundos :: [(a,b)] -> [b]
segundos = map snd

nosPrimeiros :: Eq a => a -> [(a,b)] -> Bool
nosPrimeiros x = elem x . map fst

sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos []            = (0,0,0)
sumTriplos ((x,y,z):t)   = (x+sx, y+sy, z+sz)
  where (sx,sy,sz) = sumTriplos t

-- Filtra os dígitos de uma string.
soDigitos :: [Char] -> [Char]
soDigitos = filter isDigit

-- Conta as minúsculas de uma string.
minusculas :: [Char] -> Int
minusculas = length . filter isLower

-- Converte os dígitos de uma string na lista dos seus valores.
nums :: String -> [Int]
nums = map digitToInt . filter isDigit

type Polinomio = [Monomio]
type Monomio   = (Float, Int)

-- Número de monómios de grau n.
conta :: Int -> Polinomio -> Int
conta n = length . selgrau n

-- Grau do polinómio.
grau :: Polinomio -> Int
grau [] = 0
grau p  = maximum (map snd p)

-- Monómios de um dado grau.
selgrau :: Int -> Polinomio -> Polinomio
selgrau n = filter ((== n) . snd)

-- Derivada.
deriv :: Polinomio -> Polinomio
deriv []          = []
deriv ((c,e):t)   | e == 0    = deriv t
                  | otherwise = (c * fromIntegral e, e-1) : deriv t

-- Valor do polinómio em x.
calcula :: Float -> Polinomio -> Float
calcula _ []        = 0
calcula x ((c,e):t) = c * x^e + calcula x t

-- Remove monómios de coeficiente zero.
simp :: Polinomio -> Polinomio
simp = filter ((/= 0) . fst)

-- Multiplica um monómio por um polinómio.
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) = map (\(c',e') -> (c*c', e+e'))

-- Junta monómios do mesmo grau.
normaliza :: Polinomio -> Polinomio
normaliza []        = []
normaliza ((c,e):t) = (c + s, e) : normaliza (filter ((/= e) . snd) t)
  where s = sum [c' | (c',e') <- t, e' == e]

-- Soma de dois polinómios normalizados.
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)

-- Produto de dois polinómios.
produto :: Polinomio -> Polinomio -> Polinomio
produto p1 p2 = normaliza (concatMap (`mult` p2) p1)

-- Ordena por grau crescente.
ordena :: Polinomio -> Polinomio
ordena []        = []
ordena ((c,e):t) = ordena [m | m <- t, snd m < e] ++ [(c,e)] ++ ordena [m | m <- t, snd m >= e]

-- Testa se dois polinómios são equivalentes.
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordena (simp (normaliza p1)) == ordena (simp (normaliza p2))

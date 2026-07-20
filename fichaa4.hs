import Data.Char (isDigit, intToDigit)

-- Separa uma string em (dígitos, restantes caracteres).
digitAlpha :: String -> (String, String)
digitAlpha [] = ([], [])
digitAlpha (h:t)
  | isDigit h = (h:ds, as)
  | otherwise = (ds, h:as)
  where (ds, as) = digitAlpha t

-- Conta negativos, zeros e positivos.
nzp :: [Int] -> (Int, Int, Int)
nzp [] = (0, 0, 0)
nzp (h:t)
  | h > 0     = (n, z, p+1)
  | h == 0    = (n, z+1, p)
  | otherwise = (n+1, z, p)
  where (n, z, p) = nzp t

-- Divisão inteira por subtrações sucessivas.
myDivMod :: Integral a => a -> a -> (a, a)
myDivMod _ 0 = error "Divisao por zero"
myDivMod x y
  | x < y     = (0, x)
  | otherwise = (q+1, r)
  where (q, r) = myDivMod (x-y) y

-- Converte uma lista de dígitos no inteiro correspondente (acumulador).
fromDigits :: [Int] -> Int
fromDigits = foldl (\acc d -> acc*10 + d) 0

-- Maior soma de um prefixo da lista (acumulador).
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit = aux 0 0
  where aux _    m []      = m
        aux soma m (h:t)   = aux novaSoma (max novaSoma m) t
          where novaSoma = soma + h

-- Fibonacci em tempo linear (dois acumuladores).
fib :: Int -> Int
fib n = aux n 0 1
  where aux 0 a _ = a
        aux k a b = aux (k-1) b (a+b)

-- Converte um inteiro numa string (acumulador).
intToStr :: Integer -> String
intToStr 0 = "0"
intToStr x = aux x []
  where aux 0 acc = acc
        aux n acc = aux (div n 10) (intToDigit (fromInteger (mod n 10)) : acc)

-- Exercício 8/9: listas por compreensão.
-- 8a) [x | x <- [1..20], mod x 6 == 0]
-- 8c) [(x, 30-x) | x <- [10..20]]
-- 8d) [x^2 | x <- [1..5], _ <- [1,2]]
-- 9a) [2^x | x <- [0..10]]
-- 9b) [(x, 6-x) | x <- [1..5]]
-- 9c) [[y | y <- [1..x]] | x <- [1..5]]
-- 9e) [product [1..x] | x <- [1..6]]

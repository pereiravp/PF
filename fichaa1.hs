import Data.Char (ord, chr)

-- Primeiro e último elemento de uma lista.
primUlt :: [a] -> (a, a)
primUlt l = (head l, last l)

-- Testa se m é múltiplo de n.
multiplo :: Int -> Int -> Bool
multiplo m n = mod m n == 0

-- Remove o primeiro elemento se a lista tiver comprimento ímpar.
truncaImpar :: [a] -> [a]
truncaImpar l | even (length l) = l
              | otherwise       = tail l

max2 :: Int -> Int -> Int
max2 = max

max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 a (max2 b c)

-- Número de raízes reais de ax^2 + bx + c.
nRaizes :: Double -> Double -> Double -> Int
nRaizes a b c | delta > 0  = 2
              | delta == 0 = 1
              | otherwise  = 0
  where delta = b^2 - 4*a*c

-- Lista das raízes reais.
raizes :: Double -> Double -> Double -> [Double]
raizes a b c | n == 0    = []
             | n == 1    = [-b / (2*a)]
             | otherwise = [(-b + sqrt delta) / (2*a), (-b - sqrt delta) / (2*a)]
  where n     = nRaizes a b c
        delta = b^2 - 4*a*c

data Hora = H Int Int deriving (Show, Eq)

validTime :: Hora -> Bool
validTime (H h m) = 0 <= h && h < 24 && 0 <= m && m < 60

-- Testa se a primeira hora é posterior à segunda.
ordTime :: Hora -> Hora -> Bool
ordTime (H h1 m1) (H h2 m2) = h1 > h2 || (h1 == h2 && m1 > m2)

horasMinutos :: Hora -> Int
horasMinutos (H h m) = h*60 + m

minutosHoras :: Int -> Hora
minutosHoras m = H (div m 60) (mod m 60)

difHoras :: Hora -> Hora -> Int
difHoras t1 t2 = abs (horasMinutos t1 - horasMinutos t2)

addMinutos :: Hora -> Int -> Hora
addMinutos t x = minutosHoras (horasMinutos t + x)

data Semaforo = Verde | Amarelo | Vermelho deriving (Show, Eq)

next :: Semaforo -> Semaforo
next Verde    = Amarelo
next Amarelo  = Vermelho
next Vermelho = Verde

stop :: Semaforo -> Bool
stop Vermelho = True
stop _        = False

-- Seguro se pelo menos um dos semáforos está vermelho.
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Vermelho || s2 == Vermelho

data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)

posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar r t)      = r * cos t

posy :: Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar r t)      = r * sin t

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt (x^2 + y^2)
raio (Polar r _)      = r

angulo :: Ponto -> Double
angulo (Cartesiano x y) | x > 0            = atan (y/x)
                        | x < 0 && y >= 0  = atan (y/x) + pi
                        | x < 0 && y < 0   = atan (y/x) - pi
                        | x == 0 && y > 0  = pi/2
                        | x == 0 && y < 0  = -pi/2
                        | otherwise        = 0
angulo (Polar _ t) = t

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ((posx p1 - posx p2)^2 + (posy p1 - posy p2)^2)

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show, Eq)

poligono :: Figura -> Bool
poligono (Circulo _ _) = False
poligono _             = True

vertices :: Figura -> [Ponto]
vertices (Circulo _ _)      = []
vertices (Retangulo p1 p2)  = [p1, p2, Cartesiano (posx p1) (posy p2), Cartesiano (posx p2) (posy p1)]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]

area :: Figura -> Double
area (Circulo _ r)        = pi * r^2
area (Retangulo p1 p2)    = abs (posx p1 - posx p2) * abs (posy p1 - posy p2)
area (Triangulo p1 p2 p3) = sqrt (s*(s-a)*(s-b)*(s-c))   -- fórmula de Heron
  where a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2

perimetro :: Figura -> Double
perimetro (Circulo _ r)        = 2 * pi * r
perimetro (Retangulo p1 p2)    = 2 * abs (posx p1 - posx p2) + 2 * abs (posy p1 - posy p2)
perimetro (Triangulo p1 p2 p3) = dist p1 p2 + dist p2 p3 + dist p3 p1

-- Reimplementação de funções de Data.Char.
isLower :: Char -> Bool
isLower c = ord c >= ord 'a' && ord c <= ord 'z'

isDigit :: Char -> Bool
isDigit c = ord c >= ord '0' && ord c <= ord '9'

isAlpha :: Char -> Bool
isAlpha c = isLower c || (ord c >= ord 'A' && ord c <= ord 'Z')

toUpper :: Char -> Char
toUpper c | isLower c = chr (ord c - 32)
          | otherwise = c

intToDigit :: Int -> Char
intToDigit d = chr (d + ord '0')

digitToInt :: Char -> Int
digitToInt c = ord c - ord '0'

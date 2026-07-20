data Hora = H Int Int deriving Show
type Etapa   = (Hora, Hora)
type Viagem  = [Etapa]

validTime :: Hora -> Bool
validTime (H h m) = 0 <= h && h < 24 && 0 <= m && m < 60

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

-- Numa etapa a chegada tem de ser posterior à partida.
testeEtapa :: Etapa -> Bool
testeEtapa (ini, fim) = ordTime fim ini

-- Etapas válidas e sem sobreposição temporal entre etapas consecutivas.
testeViagem :: Viagem -> Bool
testeViagem []               = True
testeViagem [e]              = testeEtapa e
testeViagem ((i1,f1):(i2,f2):t) = testeEtapa (i1,f1) && ordTime i2 f1 && testeViagem ((i2,f2):t)

-- Partida da primeira etapa e chegada da última.
calcularHora :: Viagem -> Etapa
calcularHora v = (fst (head v), snd (last v))

-- Tempo efetivo de viagem (soma das etapas).
tempoViagem :: Viagem -> Hora
tempoViagem []          = H 0 0
tempoViagem ((i,f):t)   = minutosHoras (difHoras i f + horasMinutos (tempoViagem t))

-- Tempo total de espera entre etapas consecutivas.
tempoEspera :: Viagem -> Hora
tempoEspera []                 = H 0 0
tempoEspera [_]                = H 0 0
tempoEspera ((_,f1):(i2,f2):t) = minutosHoras (difHoras f1 i2 + horasMinutos (tempoEspera ((i2,f2):t)))

-- Tempo total (viagem + espera).
tempoTotal :: Viagem -> Hora
tempoTotal []  = H 0 0
tempoTotal v   = minutosHoras (difHoras ini fim)
  where (ini, fim) = calcularHora v

type Poligonal = [Ponto]
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show, Eq)

posx :: Ponto -> Double
posx (Cartesiano x _) = x
posx (Polar r t)      = r * cos t

posy :: Ponto -> Double
posy (Cartesiano _ y) = y
posy (Polar r t)      = r * sin t

dist :: Ponto -> Ponto -> Double
dist p1 p2 = sqrt ((posx p1 - posx p2)^2 + (posy p1 - posy p2)^2)

-- Comprimento de uma linha poligonal.
comprimento :: Poligonal -> Double
comprimento []       = 0
comprimento [_]      = 0
comprimento (p:q:t)  = dist p q + comprimento (q:t)

-- Testa se a linha é fechada (primeiro e último ponto coincidem).
linhaFechada :: Poligonal -> Bool
linhaFechada []      = False
linhaFechada (p:t)   = posx p == posx (last t) && posy p == posy (last t)

data Figura = Circulo Ponto Double
            | Retangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show, Eq)

area :: Figura -> Double
area (Circulo _ r)        = pi * r^2
area (Retangulo p1 p2)    = abs (posx p1 - posx p2) * abs (posy p1 - posy p2)
area (Triangulo p1 p2 p3) = sqrt (s*(s-a)*(s-b)*(s-c))
  where a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2

-- Triangulação em leque a partir do primeiro vértice.
triangula :: Poligonal -> [Figura]
triangula (p:q:r:t) = Triangulo p q r : triangula (p:r:t)
triangula _         = []

-- Área de uma linha poligonal fechada (soma das áreas dos triângulos).
areaPoligono :: Poligonal -> Double
areaPoligono = sum . map area . triangula

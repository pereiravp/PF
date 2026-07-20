-- Programação Funcional — 1º ano
-- Reimplementação recursiva de funções pré-definidas.

-- 1.
enumFromTo_ :: Int -> Int -> [Int]
enumFromTo_ x y | x > y     = []
                | otherwise = x : enumFromTo_ (x+1) y

-- 2.
enumFromThenTo_ :: Int -> Int -> Int -> [Int]
enumFromThenTo_ x y z | x > z     = []
                      | otherwise = x : enumFromThenTo_ y (2*y - x) z

-- 3.
(+++) :: [a] -> [a] -> [a]
(+++) []    l = l
(+++) (h:t) l = h : (t +++ l)

-- 4.
(!!!) :: [a] -> Int -> a
(!!!) []    _ = error "indice fora de gama"
(!!!) (h:_) 0 = h
(!!!) (_:t) n = t !!! (n-1)

-- 5.
reverse_ :: [a] -> [a]
reverse_ []    = []
reverse_ (h:t) = reverse_ t ++ [h]

-- 6.
take_ :: Int -> [a] -> [a]
take_ n _ | n <= 0 = []
take_ _ []         = []
take_ n (h:t)      = h : take_ (n-1) t

-- 7.
drop_ :: Int -> [a] -> [a]
drop_ n l | n <= 0 = l
drop_ _ []         = []
drop_ n (_:t)      = drop_ (n-1) t

-- 8.
zip_ :: [a] -> [b] -> [(a,b)]
zip_ (h:t) (x:xs) = (h,x) : zip_ t xs
zip_ _ _          = []

-- 9.
replicate_ :: Int -> a -> [a]
replicate_ n _ | n <= 0 = []
replicate_ n x          = x : replicate_ (n-1) x

-- 10.
intersperse_ :: a -> [a] -> [a]
intersperse_ _ []    = []
intersperse_ _ [x]   = [x]
intersperse_ s (h:t) = h : s : intersperse_ s t

-- 11.
group_ :: Eq a => [a] -> [[a]]
group_ []    = []
group_ (h:t) = (h : takeWhile (== h) t) : group_ (dropWhile (== h) t)

-- 12.
concat_ :: [[a]] -> [a]
concat_ []    = []
concat_ (h:t) = h ++ concat_ t

-- 13.
inits_ :: [a] -> [[a]]
inits_ []    = [[]]
inits_ (h:t) = [] : map (h:) (inits_ t)

-- 14.
tails_ :: [a] -> [[a]]
tails_ []      = [[]]
tails_ l@(_:t) = l : tails_ t

-- 15.
heads :: [[a]] -> [a]
heads []          = []
heads ([]:t)      = heads t
heads ((h:_):t)   = h : heads t

-- 16.
total :: [[a]] -> Int
total = sum . map length

-- 17.
fun :: [(a,b,c)] -> [(a,c)]
fun = map (\(x,_,z) -> (x,z))

-- 18.
cola :: [(String,b,c)] -> String
cola = concatMap (\(s,_,_) -> s)

-- 19.
idade :: Int -> Int -> [(String,Int)] -> [String]
idade ano i = map fst . filter (\(_,nasc) -> ano - nasc >= i)

-- 20.
powerEnumFrom :: Int -> Int -> [Int]
powerEnumFrom n m = [n^k | k <- [0 .. m-1]]

-- 21.
isPrime :: Int -> Bool
isPrime n | n < 2     = False
          | otherwise = aux 2
  where aux m | m*m > n      = True
              | mod n m == 0 = False
              | otherwise    = aux (m+1)

-- 22.
_isPrefixOf :: Eq a => [a] -> [a] -> Bool
_isPrefixOf []    _      = True
_isPrefixOf _     []     = False
_isPrefixOf (x:xs) (y:ys) = x == y && _isPrefixOf xs ys

-- 23.
_isSuffixOf :: Eq a => [a] -> [a] -> Bool
_isSuffixOf xs ys = reverse xs `_isPrefixOf` reverse ys

-- 24.
_isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
_isSubsequenceOf []    _  = True
_isSubsequenceOf _     [] = False
_isSubsequenceOf (x:xs) (y:ys)
  | x == y    = _isSubsequenceOf xs ys
  | otherwise = _isSubsequenceOf (x:xs) ys

-- 25.
_elemIndices :: Eq a => a -> [a] -> [Int]
_elemIndices x = aux 0
  where aux _ [] = []
        aux i (h:t) | x == h    = i : aux (i+1) t
                    | otherwise = aux (i+1) t

-- 26.
_nub :: Eq a => [a] -> [a]
_nub []    = []
_nub (h:t) = h : _nub (filter (/= h) t)

-- 27.
_delete :: Eq a => a -> [a] -> [a]
_delete _ [] = []
_delete x (h:t) | x == h    = t
                | otherwise = h : _delete x t

-- 28.
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) l []     = l
(\\) l (y:ys) = _delete y l \\ ys

-- 29.
_union :: Eq a => [a] -> [a] -> [a]
_union l []    = l
_union l (h:t) | elem h l  = _union l t
               | otherwise = _union (l ++ [h]) t

-- 30.
_intersect :: Eq a => [a] -> [a] -> [a]
_intersect [] _    = []
_intersect (h:t) l | elem h l  = h : _intersect t l
                   | otherwise = _intersect t l

-- 31.
_insert :: Ord a => a -> [a] -> [a]
_insert x [] = [x]
_insert x (h:t) | x <= h    = x : h : t
                | otherwise = h : _insert x t

-- 32.
_unwords :: [String] -> String
_unwords []    = ""
_unwords [w]   = w
_unwords (h:t) = h ++ " " ++ _unwords t

-- 33.
_unlines :: [String] -> String
_unlines []    = ""
_unlines (h:t) = h ++ "\n" ++ _unlines t

-- 34. Posição do maior elemento (a primeira, contando de 0).
pMaior :: Ord a => [a] -> Int
pMaior [_]    = 0
pMaior (h:t)  | h >= t !! p = 0
              | otherwise   = 1 + p
  where p = pMaior t

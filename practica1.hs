import Data.List (delete)
--EJERCICIO 1

-- a)
borrarUltimo :: [a] -> [a]
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

-- b)

-- Función auxiliar que agrupa los valores para una clave dada
agrupar :: Eq k => [(k, v)] -> k -> [v]
agrupar xs k = [v | (key, v) <- xs, key == k]

-- Función collect que asocia cada clave con una lista de valores
collect' :: Eq k => [(k, v)] -> [(k, [v])]
collect' [] = []
collect' ((k, v):xs) = (k, agrupar ((k, v):xs) k) : collect' (filtrarClave xs k)

-- Función auxiliar para filtrar las claves ya procesadas
filtrarClave :: Eq k => [(k, v)] -> k -> [(k, v)]
filtrarClave [] _ = []
filtrarClave ((key, v):xs) k
  | key == k  = filtrarClave xs k
  | otherwise = (key, v) : filtrarClave xs k


  -- c)

serie :: [a] -> [[a]]
serie xs = go xs (length xs)
  where
    go _ 0 = [[]]
    go xs n = go xs (n - 1) ++ [take n xs]


-- d)

paresIguales :: [Int] -> Bool
paresIguales [] = True
paresIguales (x:xs)
  | x `elem` xs = paresIguales (delete x xs)
  | otherwise = False


-- e)

isosceles :: Int -> Int -> Int -> Bool
isosceles a b c
    | a == b || a == c || b == c = True 
    | otherwise = False 

-- f)

ror :: [a] -> Int -> [a]
ror xs 0 = xs
ror (x:xs) n = (ror xs (n-1)) ++ [x]
  
-- g)

upto :: Int -> Int -> [Int]
upto n m = go n m 0 []
    where
      go n 0 y xs = xs
      go n m y xs = (n + y) : (go n (m - 1) (y + 1) xs) 

-- h)

eco :: [a] -> [a]
eco xs = go xs ((length xs))
    where
      go xs 0 = xs
      go xs pos = (go (init xs) (pos - 1)) ++ (repetir (last xs) pos)

repetir :: a -> Int -> [a]
repetir _ 0 = []
repetir x n = x : repetir x (n - 1)

--EJERCICIO 2

-- a)

cambios :: Eq a => [a] -> [Int]
cambios xs = [i | (i, (a, b)) <- zip [0..] (zip xs (tail xs)), a /= b]

-- b)

oblongos :: [Int]
oblongos = [x*(x-1) | x <- [2..5]]

--c)

abundantes :: [Int]
abundantes = [x | x <- [1..50], x < divisoresTot x]

divisoresTot :: Int -> Int
divisoresTot n = sum [d | d <- [1..n-1], n `mod` d == 0]

-- d)

-- mismo ejercicio

--f)

expandir :: [Int] -> [Int]
expandir xs = [x | x <- xs, y <- [1..x]]

-- EJERCICIO 3

--resuelto

-- EJERCICIO 4

{-
a. foo1	Bool -> (Bool -> Bool)
b. foo2	(b -> c) -> (a -> b) -> a -> c
c. foo3	(a -> b -> c) -> a -> b -> c
d. foo4	(b -> a) -> b -> [a] -> [a]
e. foo5	a -> (b -> [a]) -> b -> [a]
f. foo6	[a] -> (b -> [a]) -> b -> [a]
g. foo7	[[a]] -> ([[a]] -> Bool) -> [a]
h. foo8	[a] -> ([a] -> Bool) -> [a]
-}

--EJERCICIO 5

--a) 
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

--b)
filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = foldr (\x acc -> if f x then x : acc else acc) [] xs

--c)
unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = foldr (\(x, y) (accA, accB) -> (x : accA, y : accB)) ([], []) xs

--d)
pair2List :: (a, [b]) -> [(a, b)]
pair2List (x, ys) = foldr (\y acc -> (x, y) : acc) [] ys

--e)
maxSec :: [(Int, Int)] -> (Int, Int)
maxSec (x:xs) = foldr (\p acc -> if segmento p > segmento acc then p else acc) x xs

segmento :: (Int, Int) -> Int
segmento (x, y) = abs (x - y)
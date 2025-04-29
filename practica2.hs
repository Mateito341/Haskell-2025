-- EJERCICIO 1
{-
-- Definición del tipo RGB como una tupla
type RGB = (Int, Int, Int)

-- Definición del tipo Color como un record
data Color = Color { red :: Int
                   , green :: Int
                   , blue :: Int
                   }

-- Función 1: Mezclar dos colores (representados como RGB)
mezclar :: RGB -> RGB -> RGB
mezclar (r1, g1, b1) (r2, g2, b2) =
    (div (r1 + r2) 2,
     div (g1 + g2) 2,
     div (b1 + b2) 2)

-- Función 2: Mezclar dos colores (representados como Color)
mezcla :: Color -> Color -> Color
mezcla (Color r1 g1 b1) (Color r2 g2 b2) =
    Color (div (r1 + r2) 2)
          (div (g1 + g2) 2)
          (div (b1 + b2) 2)

-- EJERCICIO 2

data Linea = Linea { palabra :: [Char]
                   , cursor :: Int --posición del cursor
                   }   

vacia :: Linea
vacia = Linea [] 0

moverDer :: Linea -> Linea
moverDer (Linea p c) = if c < length p then Linea p (c + 1) else Linea p c

moverIzq :: Linea -> Linea
moverIzq (Linea p c) = if c > 0 then Linea p (c-1) else Linea p c

moverIni :: Linea -> Linea
moverIni (Linea p c) = Linea p 0

moverFin :: Linea -> Linea
moverFin (Linea p c) = Linea p (length p)

insertar :: Char -> Linea -> Linea
insertar x (Linea p c) = Linea (take c p ++ [x] ++ drop c p) (c + 1)

borrar :: Linea -> Linea
borrar (Linea p cursor) = 
    if cursor > 0 
    then Linea (take (cursor - 1) p ++ drop cursor p) (cursor - 1)
    else Linea p cursor

-- EJERCICIO 3
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

--a)
headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc _ xs _) = xs

lastCL :: CList a -> a
lastCL (CUnit x) = x
lastCL (Consnoc _ _ y) = y

initCL :: CList a -> CList a
initCL (CUnit _) = EmptyCL
initCL (Consnoc x xs _) = xs

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnitCL :: CList a -> Bool
isCUnitCL (CUnit _) = True
isCUnitCL _ = False

--b)

reverseCl :: CList a -> CList a
reverseCl EmptyCL = EmptyCL
reverseCl (CUnit x) = CUnit x
reverseCl Consnoc x xs y = Consnoc y (reverseCl xs) x

--c)
-- Función para construir una CList a partir de un elemento y otra CList
consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit y) = Consnoc x EmptyCL y
consCL x (Consnoc y ys z) = Consnoc x (consCL y ys) z

-- Función para añadir un elemento al final de una CList
snocCL :: CList a -> a -> CList a
snocCL EmptyCL x = CUnit x
snocCL (CUnit x) y = Consnoc x EmptyCL y
snocCL (Consnoc x xs y) z = Consnoc x (snocCL xs y) z

initsCL :: CList a -> CList (CList a)
initsCL EmptyCL = CUnit EmptyCL
initsCL xs = snocCL (initsCL (initCL xs)) xs

--d)
lastsCL :: CList a -> CList (CList a)
lastsCL EmptyCL = CUnit EmptyCL
lastsCL xs = consCL xs (lastsCL (tailCL xs))

--e)

concatCL :: CList (CList a) -> CList a 
concatCL EmptyCL = EmptyCL
concatCL (CUnit a) = CUnit a 
concatCL (Consnoc x xs y) = x (concatCL xs) y --mal
-}


-- EJERCICIO 4

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval :: Aexp -> Int
eval (Num x) = x
eval (Prod a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b

-- EJERCICIO 5

data Bin a = Hoja | Nodo (Bin a) a (Bin a)

maximum :: Bin a → a
maximum (Nodo l a Hoja) = a
maximum (Nodo l a r ) = maximum r

minimum :: Bin a → a
minimum (Nodo Hoja a r ) = a
minimum (Nodo l a r ) = minimum l

checkBST :: (Ord a) => Bin a -> Bool
checkBST Hoja = True
checkBST (Nodo Hoja a Hoja) = True
checkBST (Nodo l a Hoja) = (maximum l) <= a && checkBST l
checkBST (Nodo Hoja a r) = (minimum r) >= a && checkBST r
checkBST (Nodo l a r) = (maximum l) <= a && (minimum r) >= a && checkBST l && checkBST r


  --EJERCICIO 6

data Tree a = Hoja | Nodo (Tree a) a (Tree a)

completo :: a -> Int -> Tree a
completo _ 0 = Hoja
completo x n = let subarbol = completo x (n-1)
               in Nodo subarbol x subarbol

-- EJERCICIO 7

member :: Ord a => a -> Bin a -> Bool
member a Hoja = False
member a (Nodo l b r ) 
  | a == b = True
  | a < b = member a l
  | a > b = member a r


memberOpt :: Ord a => a -> Bin a -> Bool
memberOpt x arbol = memberAux x arbol
  where
    memberAux :: Ord a => a -> Bin a -> Bool
    memberAux _ Hoja = False
    memberAux x (Nodo l b r) = case compare x b of
      EQ -> True      -- x == b
      LT -> memberAux x l  -- x < b
      GT -> memberAux x r  -- x > b
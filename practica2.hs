-- EJERCICIO 1

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
moverIzq (Linea p c) = if c > 0 then Linea p (c - 1) else Linea p c

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
tailCL (CUnit x) = EmptyCL
tailCL (Consnoc x EmptyCL y) = CUnit y
tailCL (Consnoc x xs y) = Consnoc (headCL xs) (tailCL xs) y

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
reverseCl (Consnoc x xs y) = Consnoc y (reverseCl xs) x

--c)
-- Función para construir una CList a partir de un elemento y otra CList
cons :: a -> CList a -> CList a
cons e EmptyCL = CUnit e 
cons e (CUnit x) = Consnoc e EmptyCL x
cons e (Consnoc x xs y) = Consnoc e (cons x xs) y

borrarUCL :: CList a -> CList a
borrarUCL EmptyCL = EmptyCL
borrarUCL (CUnit x) = EmptyCL
borrarUCL (Consnoc x xs z) = cons x xs

-- Función para añadir un elemento al final de una CList
snocCL :: CList a -> a -> CList a
snocCL EmptyCL x = CUnit x
snocCL (CUnit x) y = Consnoc x EmptyCL y
snocCL (Consnoc x xs y) z = Consnoc x (snocCL xs y) z

initsCL :: CList a -> CList (CList a)
initsCL EmptyCL = CUnit EmptyCL
initsCL xs = snocCL (initsCL (initCL xs)) xs

--d)

initsCL :: CList a -> CList (CList a)
initsCL EmptyCL = CUnit EmptyCL
initsCL xs = snoc (initsCL (borrarUCL xs)) xs

--e)
lastsCL :: CList a -> CList (CList a)
lastsCL EmptyCL = CUnit EmptyCL
lastsCL xs = snoc (lastsCL (tailCL xs)) xs

-- EJERCICIO 4

data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval :: Aexp -> Int
eval (Num x) = x
eval (Prod a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod a b) =
  case seval a of
    Nothing -> Nothing
    Just x -> case seval b of
                Nothing -> Nothing
                Just y -> Just (x * y)
seval (Div a b) =
  case seval a of
    Nothing -> Nothing
    Just x -> case seval b of
                Nothing -> Nothing
                Just 0 -> Nothing
                Just y -> Just (x `div` y)


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

-- EJERCICO 8

data Color = R | B
data RBT a = E | T Color (RBT a) a (RBT a)

balance :: Color → RBT a → a → RBT a → RBT a
balance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
balance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
balance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
balance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
balance c l a r = T c l a r

insert :: Ord a => a -> RBT a -> RBT a
insert x t = makeBlack (ins x t)
  where
    ins x E = T R E x E
    ins x (T c l y r)
      | x < y     = balance c (ins x l) y r
      | x > y     = balance c l y (ins x r)
      | otherwise = T c l y r

makeBlack :: RBT a -> RBT a
makeBlack E           = E
makeBlack (T _ l x r) = T B l x r

ffromOrdList :: [a] -> RBT a
ffromOrdList xs = go xs E -- lista arbol 
  where
    go [] t = t 
    go (x:xs) t n = go xs (insert x t)
  
data Color = R | B deriving(Show, Eq)
data RBT a = E | T Color (RBT a) a (RBT a) deriving(Show)

fromOrdList' :: Ord a => [a] -> Color -> RBT a 
fromOrdList' [] _ = E 
fromOrdList' xs c = let m    = div (length xs) 2 
                        x    = xs !! m -- x es el elemento ed la mitad de la lista
                        ant  = take m xs
                        post = drop (m+1) xs
                        c'   = if c == R then B else R -- garantiza que se alterne entre negro y rojo
                        in T c (fromOrdList' ant c') x (fromOrdList' post c')

fromOrdList :: Ord a => [a] -> RBT a
fromOrdList xs = fromOrdList' xs B

--EJERCICIO 9
-- balancea si el problema está en el subárbol izquierdo
lbalance :: Color -> RBT a -> a -> RBT a -> RBT a
lbalance B (T R (T R a x b) y c) z d = T R (T B a x b) y (T B c z d)
lbalance B (T R a x (T R b y c)) z d = T R (T B a x b) y (T B c z d)
lbalance c l a r = T c l a r

-- balancea si el problema está en el subárbol derecho
rbalance :: Color -> RBT a -> a -> RBT a -> RBT a
rbalance B a x (T R (T R b y c) z d) = T R (T B a x b) y (T B c z d)
rbalance B a x (T R b y (T R c z d)) = T R (T B a x b) y (T B c z d)
rbalance c l a r = T c l a r


insert' :: Ord a => a -> RBT a -> RBT a
insert' x t = makeBlack (ins x t)
  where
    ins x E = T R E x E
    ins x (T c l y r)
      | x < y     = lbalance c (ins x l) y r
      | x > y     = rbalance c l y (ins x r)
      | otherwise = T c l y r

-- EJERCICIO 10
type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a)

merge :: Ord a => Heap a -> Heap a -> Heap a
merge h1 E = h1
merge E h2 = h2
merge h1@(N x a1 b1) h2@(N y a2 b2) =
  if x <= y
    then makeH x a1 (merge b1 h2)
    else makeH y a2 (merge h1 b2)


rank :: Heap a → Rank
rank E = 0
rank (N r _ _ _) = r

makeH :: a -> Heap a -> Heap a -> Heap a
makeH x a b = if rank a > rank b then N (rank b + 1) x a b
else N (rank a + 1) x b a

fromList :: [a] -> Heap a 
fromList xs = 
  let ys = map (\x -> N 1 x E E) xs
      pares [] = []
      pares [x] = [x]
      pares (x:y:hs) = merge x y : pares hs
      g [h] = h
      g hs = g (pares hs)
  in g ys

-- Ejercicio 1

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


--EJERCICIO 2
type Cadena = [Char]
type Linea = (Cadena, Cadena)

insertar :: Char -> Linea -> Linea
insertar c (ls, rs) = (c:ls, rs)

moverIzq :: Linea -> Linea
moverIzq ("" , rs) = ("", rs)
moverIzq (c:ls, rs) = (ls , c:rs)

moverDer :: Linea -> Linea
moverDer (ls , []) = (ls , [])
moverDer (ls , c:rs) = (c:ls, rs)

moverIni :: Linea -> Linea
moverIni (ls , rs) = ([], reverse ls ++ rs)

moverFin :: Linea -> Linea
moverFin (ls , rs) = (reverse rs ++ ls, [])

--EJERCICIO 3

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a

headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x _ _) = x

tailCL :: CList a -> CList a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc _ EmptyCL _) = EmptyCL
tailCL (Consnoc _ xs _) = xs

isEmptyCL :: Clist a -> Bool
isEmpety (EmptyCL) = True
isEmpety _ = False

isUnit :: Clist a -> Bool
isUnit (CUnit a) = True
isUnit _ = False

--EJERCICIO 3.1 (profe)

-- `cons` inserta un elemento al principio de la lista
cons :: a -> Clist a -> Clist a
cons e EmptyCL = CUnit e
cons e (CUnit x) = Consnoc e EmptyCL x
cons e (Consnoc x xs y) = Consnoc e (cons e xs) y

-- `snoc` agrega un elem-- `initsCL` obtiene todas las listas iniciales de una lista `Clist`
snoc (CUnit x) e = Consnoc x EmptyCL e
snoc (Consnoc x xs y) e = Consnoc x (snoc xs e) y

-- `borrarCl` elimina el primer elemento de la lista `Clist`
borrarCl :: Clist a -> Clist a
borrarCl EmptyCL = EmptyCL
borrarCl (CUnit _) = EmptyCL
borrarCl (Consnoc _ xs _) = xs

initsCL :: Clist a -> Clist (Clist a)
initsCL EmptyCL = CUnit EmptyCL 
initsCL xs = snoc (initsCL (borrarCl xs)) xs  

pegar :: Clist (Clist a) -> Clist (Clist a) -> Clist (Clist a)
pegar xs EmptyCL = xs
pegar EmptyCL ys = ys
pegar (Consnoc x xs z) (Consnoc y ys h) = Consnoc x (pegar(sonc xs z) (cons y ys)) h

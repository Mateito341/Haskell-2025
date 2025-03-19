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

insertar c (ls, rs) = (c:ls,rs)

moverIzq ("" , rs) = ("" , rs)
moverIzq (c:ls, rs) = (ls ,c:rs)
moverDer (ls , []) = (ls ,[] )  git config --global user.email "you@example.com"

moverDer (ls ,c:rs) = (c:ls, rs)

moverIni (ls , rs) = ([], rev ls [] ++rs)
moverFin (ls , rs) = (rev rs []++ls ,[])

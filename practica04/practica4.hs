{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int
longitud ArbolVacio = 0
longitud (Raiz _ izq der) = 1 + longitud izq + longitud der

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int
profundidad ArbolVacio = 0
profundidad (Raiz _ izq der) = 1 + max (profundidad izq) (profundidad der)

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int
ancho ArbolVacio = 0
ancho (Raiz _ ArbolVacio ArbolVacio) = 1
ancho (Raiz _ izq der) = ancho izq + ancho der

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PostOrder --modifique la firma porque originalmente decia PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
-- Caso InOrder
recorrido ArbolVacio _ = []
recorrido (Raiz valor izq der) InOrder =
    recorrido izq InOrder ++ [valor] ++ recorrido der InOrder
-- Caso PreOrder
recorrido (Raiz valor izq der) PreOrder =
    [valor] ++ recorrido izq PreOrder ++ recorrido der PreOrder
-- Caso PostOrder
recorrido (Raiz valor izq der) PostOrder =
    recorrido izq PostOrder ++ recorrido der PostOrder ++ [valor]

-------------------- EJERCICIO 5 --------------------
-- Función auxiliar para combinar niveles de los subárboles
combinarNiveles :: [[a]] -> [[a]] -> [[a]]
combinarNiveles [] ys = ys
combinarNiveles xs [] = xs
combinarNiveles (x:xs) (y:ys) = (x ++ y) : combinarNiveles xs ys

-- Función niveles
niveles :: Arbol a -> [[a]]
niveles ArbolVacio = []
niveles (Raiz valor izq der) = [ [valor] ] ++ combinarNiveles (niveles izq) (niveles der)


-------------------- EJERCICIO 6 --------------------
minimo :: Ord a => Arbol a -> a
minimo ArbolVacio = error "No existe el minimo de un arbol vacio"
minimo (Raiz valor ArbolVacio ArbolVacio) = valor
minimo (Raiz valor izq der) = menor valor minizq minder
    where
        minizq = case izq of
                 ArbolVacio -> valor
                 _ -> minimo izq
        minder = case der of
                 ArbolVacio -> valor
                 _ -> minimo der
        menor a b c
          | a<=b && b<=c = a
          | b<=a && a<=c = b
          | otherwise = c

-------------------- EJERCICIO 7 --------------------
maximo :: Ord a => Arbol a -> a
maximo ArbolVacio = error "No esiste el maximo de un arbol vacio"
maximo (Raiz valor ArbolVacio ArbolVacio) = valor
maximo (Raiz valor izq der) = mayor valor maxizq maxder
    where
        maxizq = case izq of
                 ArbolVacio -> valor
                 _ -> maximo izq
        maxder = case der of
                 ArbolVacio -> valor
                 _ -> maximo der
        mayor a b c
            |a>=b && b>=c = a
            |b>=a && a>=c = c
            |otherwise = c

-------------------- EJERCICIO 8 --------------------
eliminar ::Eq a => Ord a => Arbol a -> a -> Arbol a
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz valor izq der) num
    |valor > num = Raiz valor (eliminar izq num) der
    |valor < num = Raiz valor (eliminar der num) izq
    |otherwise = ArbolVacio


{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
data Arbol a = ArbolVacio | Raiz a (Arbol a) (Arbol a) deriving Show

-------------------- EJERCICIO 1 --------------------
longitud :: Arbol a -> Int 
longitud = undefined

-------------------- EJERCICIO 2 --------------------
profundidad :: Arbol a -> Int 
profundidad = undefined

-------------------- EJERCICIO 3 --------------------
ancho :: Arbol a -> Int 
ancho = undefined

-------------------- EJERCICIO 4 --------------------
data Recorrido = InOrder | PreOrder | PosOrder

recorrido :: Arbol a -> Recorrido -> [a]
recorrido = undefined

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
minimo (Raiz valor izq der) = minimum [valor, minimo izq, minimo der]

-------------------- EJERCICIO 7 --------------------
maximo :: Ord a => Arbol a -> a 
maximo ArbolVacio = error "No esiste el maximo de un arbol vacio"
maximo (Raiz valor ArbolVacio ArbolVacio) = valor
maximo (Raiz valor izq der) = maximum[valor, maximo izq, maximo der]

-------------------- EJERCICIO 8 --------------------
eliminar ::Eq a => Ord a => Arbol a -> a -> Arbol a 
eliminar ArbolVacio _ = ArbolVacio
eliminar (Raiz valor izq der) num
    |valor > num = Raiz valor (eliminar izq num) der
    |valor < num = Raiz valor (eliminar der num) izq
    |otherwise = ArbolVacio
 

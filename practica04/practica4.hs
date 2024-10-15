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
niveles :: Arbol a -> [[a]]
niveles = undefined

-------------------- EJERCICIO 6 --------------------
minimo :: Arbol a -> a 
minimo = undefined

-------------------- EJERCICIO 7 --------------------
maximo :: Arbol a -> a 
maximo = undefined

-------------------- EJERCICIO 8 --------------------
eliminar :: Ord a => Arbol a -> a -> Arbol a 
eliminar = undefined

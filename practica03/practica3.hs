import Control.Exception (ErrorCall(ErrorCall))
import Distribution.Simple.GHC (installExe)
data List a = Void | Node a (List a) deriving Show

longitud :: List a -> Int
longitud Void = 0
longitud (Node a b) = 1 + longitud b

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void _ = False
estaContenido (Node a b) c
    |a == c = True 
    |otherwise = estaContenido b c

convertirAEstructura :: [a] -> List a
convertirAEstructura []= Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node a lista) = a : convertirALista lista

conjunto :: Eq a => List a -> List a
conjunto Void = Void
conjunto (Node a lista) 
    | estaContenido (conjunto lista) a = conjunto lista
    | otherwise = Node a (conjunto lista)


eliminarIndice :: List a -> Int -> List a
eliminarIndice (Node a lista) 0 = lista
eliminarIndice Void _ = error "La lista esta vacia"
eliminarIndice (Node a lista) i 
    | i<0 || i>longitud(Node a lista) = error"El indice esta fuera del rango permitido"
    |otherwise = Node a (eliminarIndice lista (i-1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice lista 0 nw = Node nw lista
insertarIndice (Node a lista) i nw
    |i>longitud(Node a lista) || i<0 = error"indice fuera de rango"
    |otherwise = Node a (insertarIndice lista (i-1) nw)
insertarIndice Void _ nw = Node nw Void

recorrerALaIzquierda :: List a -> Int -> List a
recorrerALaIzquierda Void _ = Void 
recorrerALaIzquierda lista 0 = lista 
recorrerALaIzquierda (Node a lista) n = recorrerALaIzquierda (insertarIndice lista (longitud lista) a) (n-1)
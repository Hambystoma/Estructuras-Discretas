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
eliminarIndice = undefined

insertarIndice :: List a -> Int -> a -> List a
insertarIndice = undefined

recorrerLista :: List a -> Int -> List a
recorrerLista = undefined
import Control.Exception (ErrorCall(ErrorCall))
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
eliminarIndice l@(Node a lista) i = if i < 0 || i > longitud l -1 
    then error "Indice fuera del rango permitido"
    else if i==0 
        then  lista
        else Node a (eliminarIndice lista (i-1))

insertarIndice :: List a -> Int -> a -> List a
insertarIndice l@(Node a lista) i nw = if (i < 0 || i > longitud l -1)
    then error "Indice fuera del rango permitido"
    else if i==0
        then Node nw l 
        else Node a (insertarIndice lista (i-1) nw) 

recorrerLista :: List a -> Int -> List a
recorrerLista Void _ = Void 
recorrerLista lista 0 = lista 
recorrerLista lista n = recorrerLista (Node (ultimo lista) (mvUltimo lista)) (n - 1)
  where
    ultimo (Node a Void) = a
    ultimo (Node _ lista) = ultimo lista
    
    mvUltimo (Node _ Void) = Void
    mvUltimo (Node a lista) = Node a (mvUltimo lista)
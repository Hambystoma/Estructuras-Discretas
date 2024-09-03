import Control.Exception (ErrorCall(ErrorCall))
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

sumaLista :: Num a => [a] -> Int
sumaLista _ = undefined

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento (x:xs) y bool
    | bool = y:(x:xs)
    | otherwise = (x:xs)++[y]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista _ = undefined

indice :: [a] -> Int -> a
indice [] _ = error "indice fuera de rango"
indice (x:xs) 0 = x
indice (x:xs) n = indice xs (n-1)

divisores :: Int -> [Int]
divisores _ = undefined

estaEnLista ::Eq a => [a] -> a -> Bool
estaEnLista [] _ = False
estaEnLista (x:xs) n = indice (x:xs) 0 == n || estaEnLista xs n

eliminarDeLista :: Eq a => [a] -> a -> [a]
eliminarDeLista [] _ = []
eliminarDeLista (x:xs) n
    | x == n = eliminarDeLista xs n
    | otherwise = x:eliminarDeLista xs n

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs)
    | not (estaEnLista (x:xs) z) = x:xs
    | estaEnLista (x:xs) z = x:conjunto (eliminarDeLista xs z)
        where z = indice (x:xs) 0

numerosPares :: Num a => [a] -> [a]
numerosPares _ = undefined
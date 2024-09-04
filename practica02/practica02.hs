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

estaEnLista :: Eq a => [a] -> a -> Bool
estaEnLista xs n = not (null [x | x <- xs, x == n])

eliminarDeLista :: Eq a => [a] -> a -> [a]
eliminarDeLista xs n = [x | x <- xs, x /= n]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto (eliminarDeLista xs x)


numerosPares :: Num a => [a] -> [a]
numerosPares _ = undefined
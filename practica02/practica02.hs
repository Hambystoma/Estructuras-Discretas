import Control.Exception (ErrorCall(ErrorCall))
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

sumaLista :: Num a => [a] -> Int
sumaLista [] = 0

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

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto[y | y <- xs, y/=x]


numerosPares :: Num a => [a] -> [a]
numerosPares _ = undefined
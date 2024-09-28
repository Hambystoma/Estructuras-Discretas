import Control.Exception (ErrorCall(ErrorCall))
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

sumaLista :: Num a => [a] -> a
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs

agregaElemento :: [a] -> a -> Bool -> [a]
agregaElemento x y bool
    | bool = y:x
    | otherwise = x++[y]

maximoLista :: (Num a, Ord a) => [a] -> a
maximoLista [x] = x
maximoLista (x:xs)
    | x > maximoLista xs = x
    | otherwise = maximoLista xs

indice :: [a] -> Int -> a
indice [] _=error "se introdujo una lista vacia"
indice (x:xs) n
    |0>n || n>longitud (x:xs)-1= error "indice fuera de rango"
    |n==0=x
    |otherwise=indice xs (n-1)
divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

conjunto :: Eq a => [a] -> [a]
conjunto [] = []
conjunto (x:xs) = x : conjunto [y | y <- xs, y/=x]


numerosPares :: [Int] -> [Int]
numerosPares xs = [x | x <- xs, x `mod` 2 == 0]


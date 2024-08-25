distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (x, y) (x2, y2) = sqrt ((x2-x)^2+(y2-y)^2)

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt((a)^2 + (b)^2) 

pendiente :: (Float, Float) -> (Float, Float) -> Float
pendiente (x, y) (x2, y2) = (y2-y)/(x2-x)

raices :: Float -> Float -> Float -> (Float, Float)
raices a b c = (x1, x2)
    where
        x1 = (-b + sqrt(b*b-4*a*c))/(2*a)       
        x2 = (-b + sqrt(b*b-4*a*c))/(2*a)       

areaTriangulo ::  Float -> Float -> Float -> Float
areaTriangulo a b c= sqrt(((a+b+c)/2)*(((a+b+c)/2)-a)*(((a+b+c)/2)-b)*(((a+b+c)/2)-c))

comparador :: Int -> Int -> Int
comparador x y
  | x == y = 0
  | x > y = 1
  | otherwise = -1

maximo :: Int -> Int -> Int -> Int
maximo x y z
    | (x > y) && (x > z) = x
    | (y > x) && (y > z) = y
    | (z > x) && (z > y) = z
    | otherwise = x

esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w
    | w < z && z < y && y < x = True
    | otherwise = False
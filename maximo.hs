maximoEntreTresNumeros :: Int -> Int -> Int -> Int
maximoEntreTresNumeros x y z
    | (x > y) && (x > z) = x
    | (y > x) && (y > z) = y
    | (z > x) && (z > y) = z
    | otherwise = x
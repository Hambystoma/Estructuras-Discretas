esDescendente :: Int -> Int -> Int -> Int -> Bool
esDescendente x y z w
    | w < z && z < y && y < x = True
    | otherwise = False
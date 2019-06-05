module Test where
sumador :: Int -> Int -> Int
sumador x y = x + y 

division :: Int -> Int -> Int
division x y = x `div` y

maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y

cabeza :: [Int] -> [Int]
cabeza [] = error "No puedes usar una lista vacia"
cabeza (x:xs) = x
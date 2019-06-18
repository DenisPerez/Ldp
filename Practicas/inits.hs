inits :: [Int] -> [[Int]]
inits [] = [[]]
inits (x:xs) = [] : map (x:) (inits xs)
--toma una lista de la forma [8,5,5] y la convierte en [[],[8],[8,5],[8,5,5]]
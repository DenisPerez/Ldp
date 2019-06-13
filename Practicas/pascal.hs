module Pascal where
    pascal :: Int -> [Int]
    pascal 1 = [1]
    pascal 2 = [1,1]
    pascal n = [1] ++ (sum (pascal (n-1))) ++ [1] where
                            sum [x,y] = [x+y]
                            sum (x:(y:ys)) = (x+y):(sum (y:ys))
    
    triangulo :: Int -> [[Int]]
    triangulo 1 = [[1]]
    triangulo n = pascal n : triangulo (n-1)

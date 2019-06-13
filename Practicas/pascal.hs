module Pascal where
    pascal :: Int -> [Int]
    pascal 1 = [1]
    pascal 2 = [1,1]
    pascal n = [1] ++ (sumPares (pascal (n-1))) ++ [1] where
                            sumPares [x,y] = [x+y]
                            sumPares (x:(y:ys)) = (x+y):(sumPares (y:ys))
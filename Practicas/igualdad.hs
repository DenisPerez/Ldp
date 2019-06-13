module Igualdad where
    igualdad :: [Int] -> [Int] -> Bool
    igualdad [] [] = True
    igualdad [] xs = False
    igualdad xs [] = False
    igualdad (x:xs) (y:ys) = x==y && igualdad xs ys
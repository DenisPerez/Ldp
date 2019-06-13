module Igualdad where
    igualdad :: [Int] -> [Int] -> Bool
    igualdad [] [] = True
    igualdad [] xs = False
    igualdad xs [] = False
    igualdad (x:xs) (y:rs) = x==y && igualdad xs rs
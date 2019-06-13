module Igualdad where
    igualdad :: [Int] -> [Int] -> Bool
    igualdad [] [] = True
    igualdad [] xs = False
    igualdad xs [] = False
    igualdad (x:xs) (y:rks) = x==y && igualdad xs rks
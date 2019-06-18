module Union where
    union ::  [Int] -> [Int] -> [Int]
    union xs [] = xs
    union [] xs = xs
    union primera_lista segunda_lista = [x| x<-primera_lista , elem x segunda_lista || not (elem x segunda_lista)] ++ [y| y<-segunda_lista, not (elem y primera_lista)]
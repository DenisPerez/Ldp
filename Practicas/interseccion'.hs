module Interseccion where
    interseccion' :: [Int] -> [Int] -> [Int]
    interseccion' [] xs = xs
    interseccion' xs [] = xs
    interseccion' xs ys = [ x | x<-xs ,elem x ys]
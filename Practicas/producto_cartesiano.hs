module Producto_cartesiano where
    producto_cartesiano :: [Int] -> [Int] -> [(Int,Int)]
    producto_cartesiano xs ys = [(x,y)| x<-xs , y<-ys]
module Permutaciones where
import Data.List

permutacion :: Eq a => [a] -> [[a]]
permutacion [] = [[]]
permutacion xs = [x:ys| x<-xs, ys<-permutacion (delete x xs)]

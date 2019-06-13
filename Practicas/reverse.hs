module Reverse where
        reverse' :: [[Int]] -> [[Int]]
        reverse' [[]] = [[]]
        reverse' [[x]] = [[x]]
        reverse' (x:xs) = (reverse' xs) ++ [x]
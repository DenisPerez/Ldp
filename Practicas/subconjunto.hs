module Subconjunto where
    subconjunto :: [Int] -> [Int] -> Bool
    subconjunto [] xs = True
    subconjunto xs [] = True
    subconjunto (x:xs) ys = elem x ys && subconjunto xs ys
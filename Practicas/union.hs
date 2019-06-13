module Union where
    union :: [Int]->[Int]->[Int]
    union [] xs = []
    union xs [] = xs
    union xs ys = [x | x<-xs, elem x xs && elem x ys]
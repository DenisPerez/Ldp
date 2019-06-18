dropEvery :: Int -> [a] -> [a]
dropEvery 0 xs = xs
dropEvery 1 xs = []
dropEvery _ [] = []
dropEvery k xs = take (k-1) xs ++ dropEvery k (drop k xs)
--Elimina un ocurrencia de una cadena cada cierto n
-- dropEvery 3 ['a','b','c','d','e','f','g','h','i','k']
--"abdeghk"
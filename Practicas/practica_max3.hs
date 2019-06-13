module Max3 where
    max2 :: Int->Int->Int
    max2 x y = if x > y then x else y

    max3 :: Int -> Int->Int->Int
    max3 x y z = max2 x (max2 y z)
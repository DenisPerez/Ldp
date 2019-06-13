module Abs where
    sign :: Int -> Bool
    sign x = if x > 0 then True else False

    abs' :: Int->Int
    abs' x = if sign(x) then x else x*(-1)
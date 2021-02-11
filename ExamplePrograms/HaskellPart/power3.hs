power3 :: Int -> Int 
power3 x
    | x < 0 = -1
    | x == 0 = 1
    | otherwise = 3 * power3 (x - 1)

power2 :: Int -> Int 
power2 n 
    | n == 0 = 1
    | n `mod` 2 == 0 = (2^(n `div` 2))^2
    | otherwise = ((2^(2*(n + 1)))^2) * 2 )


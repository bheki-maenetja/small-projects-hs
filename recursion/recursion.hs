power2::Int->Int
power2 x 
    | x == 0 = 1
    | x > 0 = power2(x-1) * 2
    | otherwise = 0

fac::Int->Int
fac n
    | n == 0 = 1
    | n > 0 = n * fac(n-1)
    | otherwise = 0

sumFacs::Int->Int
sumFacs n
    | n == 0 = 1
    | n >= 0 = fac(n) + sumFacs(n-1)

sumdown::Int->Int
sumdown n
    | n == 0 = 0
    | n > 0 = sumdown(n-1) + n
    | otherwise = 0
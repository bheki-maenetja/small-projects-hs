-- PRACTICAL 2
-- 1. Define a function sumsq which takes a positive Int value n as its argument and returns the sum of the squares of the first n integers
sumsq::Int->Int
sumsq n = sum[i^2 | i <- [1..n]]

-- recursive solution
r_sumsq::Int->Int
r_sumsq n
    | n == 0 = 0
    | otherwise = n^2 + r_sumsq (n-1)

-- 2. Define a function prodsum that takes a positive Int value n and returns the product of positive odd integers not exceeding n and the sum of the positive even integers not exceeding n multiplied together
prodsum::Int->Int
prodsum n = product[i | i <- [1..n], i `mod` 2 /= 0]  * sum[i | i <- [0..n], i `mod` 2 == 0]

-- 3. Define a function isitprime that takes a positive Int value n and returns True if n is a prime number and False if it is not
isitprime::Int->Bool
isitprime n 
    | length [i | i <- [1..n], n `mod` i == 0] == 2 = True
    | otherwise = False

-- Function to generate to test isitprime
genPrimes::Int->[(Int, Bool)]
genPrimes n = [(i, isitprime i) | i <- [1..n]]

-- 4. Define a function comb :: Integer->Integer-> Integer which takes positive integers n and m. If n<m returns zero, and if n>= m returns the number of combinations of n objects taken m at a time: comb n m = n! / m! × (n − m)!
comb::Int->Int->Int
comb n m
    | n < m = 0
    | n >= m = fac n `div` (fac m * fac (n - m))
    | otherwise = 0

-- Helper function to calculate factorials
fac::Int->Int
fac n
    | n == 0 = 1
    | n > 0 = n * fac(n-1)
    | otherwise = 0
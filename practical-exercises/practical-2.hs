-- PRACTICAL 2
-- 1. Define a function sumsq which takes a positive Int value n as its argument and returns the sum of the squares of the first n integers
sumsq::Int->Int
sumsq n = sum[i^2 | i <- [1..n]]

r_sumsq::Int->Int -- recursive solution
r_sumsq n
    | n == 0 = 0
    | otherwise = n^2 + r_sumsq (n-1)

-- 2. Define a function prodsum that takes a positive Int value n and returns the product of positive odd integers not exceeding n and the sum of the positive even integers not exceeding n multiplied together
prodsum::Int->Int
prodsum n = product[i | i <- [1..n], i `mod` 2 /= 0]  * sum[i | i <- [0..n], i `mod` 2 == 0]

prodOddNums::Int->Int -- product of all odd number up to n
prodOddNums n
    | n == 0 = 1
    | n `mod` 2 /= 0 = n * prodOddNums (n-1)
    | otherwise = 1 * prodOddNums(n-1)

addEvenNums::Int->Int -- sum of all even numbers up to n
addEvenNums n
    | n == 0 = 0
    | n `mod` 2 == 0 = n + addEvenNums (n-1)
    | otherwise = 0 + addEvenNums(n-1)

new_prodsum::Int->Int -- alternative solution
new_prodsum n = prodOddNums n * addEvenNums n

-- 3. Define a function isitprime that takes a positive Int value n and returns True if n is a prime number and False if it is not
isitprime::Int->Bool
isitprime n 
    | length [i | i <- [1..n], n `mod` i == 0] == 2 = True
    | otherwise = False

getFactors::(Int,Int,[Int])->[Int] -- helper function to get factors
getFactors (n, i, fs)
    | i == 1 = 1:fs
    | n `mod` i == 0 = getFactors (n, (i-1), i:fs)
    | otherwise = getFactors(n, (i-1), fs)

new_isitprime::Int->Bool -- alternative solution
new_isitprime n
    | length (getFactors (n, n, [])) == 2 = True
    | otherwise = False

genPrimes::Int->[(Int, Bool)] -- helper function to generate to test isitprime
genPrimes n = [(i, new_isitprime i) | i <- [1..n]]

-- 4. Define a function comb :: Integer->Integer-> Integer which takes positive integers n and m. If n<m returns zero, and if n>= m returns the number of combinations of n objects taken m at a time: comb n m = n! / m! × (n − m)!
comb::Int->Int->Int
comb n m
    | n < m = 0
    | n >= m = fac n `div` (fac m * fac (n - m))
    | otherwise = 0

fac::Int->Int -- helper function to calculate factorials
fac n
    | n == 0 = 1
    | n > 0 = n * fac(n-1)
    | otherwise = 0
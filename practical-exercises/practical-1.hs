-- PRACTICAL 1
-- 1. Define a function next (using guarded equations) that takes a positive number greater than one. If it is even, divide it by two, otherwise multiply it by two and add one
next::Int->Int
next x
    | x `mod` 2 == 0 = x `quot` 2
    | otherwise = 2*x + 1

-- 2. Define a function k (using guarded equations) that takes two one of them is negate

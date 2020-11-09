-- PRACTICAL 4
-- 1. Given a list of lists of ints, sum the length of inner lists
sumLength::[[Int]]->Int
sumLength xs = sum[length x | x <- xs]

-- 2. Define a function that returns sum of all values of a list, but every second value is negated
sumAlternatingSigns::[Int]->Int
sumAlternatingSigns (x:xs)
    | xs == [] = 0
    | length xs `mod` 2 == 0 = x + sumAlternatingSigns(xs)
    | otherwise = -x + sumAlternatingSigns(xs)

-- 3. The scalar product of two lists of integers xs and ys of length n is given by the sum of the products of the corresponding integers. Using a list comprehension, define a function that returns the scalar product of two lists
scalarproduct::[Int]->[Int]->Int
scalarproduct xs ys = sum[ fst x * snd x | x <- zip xs ys]

-- 4. A positive integer is perfect if it equals the sum of all its factors, excluding the number itself. Using a list comprehension, define a function that returns the list of all perfect numbers up to a given limit
getFactors::(Int,Int,[Int])->[Int] -- helper function to get factors
getFactors (n, i, fs)
    | i == 1 = 1:fs
    | n `mod` i == 0 = getFactors (n, (i-1), i:fs)
    | otherwise = getFactors(n, (i-1), fs)

perfects::Int->[Int]
perfects num = [x | x <-[1..num], sum (getFactors (x, x, [])) - x == x ]
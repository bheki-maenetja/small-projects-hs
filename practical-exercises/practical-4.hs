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
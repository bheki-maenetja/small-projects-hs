-- PRACTICAL 6
-- 1. Define the following library functions using recursion:
-- a) Decide if all logical values in a list are true.
and2::[Bool]->Bool
and2 [True] = True
and2 [False] = False
and2 (b:bs)
    | b == True = and2 bs
    | otherwise = False

-- b) Concatenate a list of lists. 
concat2:: [[Int]] -> [Int]
concat2 [[]] = []
concat2 [a] = a
concat2 (x:xs) = x ++ concat2 xs

-- c) Produce a list with n identical elements. 
replicate2:: Int -> Int -> [Int]
replicate2 0 _ = []
replicate2 x y = [y] ++ replicate (x-1) y 

-- d) Select the nth element of a list. 
select:: [Int] -> Int -> Int
select (x:xs) n 
    | n <= 1 = x
    | n > length xs = head (reverse xs)
    | otherwise = select xs (n-1)

-- 2. A triple (x, y, z) of positive integers is called Pythagorean if x^2 + y^2 = z^2. Using a list comprehension, define a function that maps an integer n to all such triples with components in [1..n]
pyths::Int->[(Int,Int,Int)]
pyths n = [(a,b,n) | a <- [1..n], b <- [1..n], a*a + b*b == n*n ]

-- 3. Define a function groups :: Int -> [Int] -> [[Int]] that given a number k and a list, chops the list up into sublists of length k (the length of the last element of the resulting list might be less than k)
groups::Int->[Int]->[[Int]]
groups _ [] = []
groups n xs
    | n < 1 = [xs]
    | otherwise = (take n xs):[] ++ groups n (drop n xs)


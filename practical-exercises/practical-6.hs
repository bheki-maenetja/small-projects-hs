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


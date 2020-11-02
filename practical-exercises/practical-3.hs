-- PRACTICAL 3
-- 1. What are the types of the following values?
-- a) [Char, Char, Char]
-- b) (Char, Char, Char)
-- c) [(Bool, Char)]
-- d) ([Bool], [Char])
-- e) (Bool, [Char])
-- f) (Char, (Bool, Char))

-- 2. Define a function dayshoursmins which takes a non- negative Int value, representing a total number of minutes, and returns a triple that gives (days, hours, minutes) that this corresponds to
dayshoursmins::Int ->(Int,Int,Int)
dayshoursmins x = (x `div` 1440, (x `mod` 1440) `div` 60, (x `mod` 1440) `mod` 60)

-- 3. Define a function sumnegpos which takes a list of Int values and returns a pair comprising the sum of the negative and the sum of the positive integers in the list
sumnegpos::[Int]->(Int, Int)
sumnegpos xs = (sum[x | x <- xs, x < 0], sum[x | x <- xs, x > 0])

-- 4. Remove the first two and the last two elements from a list
drop2::[Int]->[Int]
drop2 xs
    | length xs >= 4 = drop 2 (reverse (drop 2 (reverse xs)))
    | otherwise = []

-- 5. Write a function shuffle(including its signature) that takes as argument a non-empty list of Ints, removes the first element and appends it at the back
shuffle::[Int]->[Int]
shuffle xs = tail xs ++ take 1 xs

-- 6. Consider a function safetail that behaves in the same way as tail, except that safetail maps the empty list to the empty list, whereas tail gives an error in this case. Define safetail (for a list of Ints) using: (a) a conditional expression, (b)guarded equations, (c) pattern matching
safetail::[Int]->[Int]
-- a)
safetail xs = if length xs > 0 then tail xs else []
-- b)
safetail xs
    | length xs > 0 = tail xs
    |otherwise = []
-- c)
safetail (_:xs) = xs 
safetail [] = []
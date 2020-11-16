-- PRACTICAL 5
-- 1. Insert an element at a given position into a list of ints
insertAt::Int->[Int]->Int->[Int]
insertAt x [] _ = [x]
insertAt n xs i = take (i-1) xs ++ [n] ++ drop (i-1) xs

-- 2. Define a recursive function that merges two sorted lists of values to give a single sorted list
merge::[Int]->[Int]->[Int]
merge [] [] = []
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
    | x <= y = [x] ++ merge xs (y:ys)
    | otherwise = [y] ++ merge (x:xs) ys
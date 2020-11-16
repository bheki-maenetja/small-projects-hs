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

-- 3. Define a recursive function msort that implements merge sort, which can be specified by the following two rules:
-- (1) Lists of length smaller than or equal to 1 are already sorted;
-- (2) Other lists can be sorted by sorting the two halves and merging the resulting list

msort::[Int]->[Int]
msort [] = []
msort xs
    | length xs == 2 = merge (take 1 xs) (drop 1 xs)
    | otherwise = merge (msort (take (length xs `div` 2) xs))  (msort (drop (length xs `div` 2) xs))
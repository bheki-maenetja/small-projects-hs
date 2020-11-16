-- PRACTICAL 5
-- 1. Insert an element at a given position into a list of ints
insertAt::Int->[Int]->Int->[Int]
insertAt x [] _ = [x]
insertAt n xs i = take (i-1) xs ++ [n] ++ drop (i-1) xs
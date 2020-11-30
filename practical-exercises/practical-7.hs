-- PRACTICAL 7
-- 1. Consider a polymorphic function dropEvery that drops every n'th element from a list
-- a) list comprehension
dropEvery::[a]->Int->[a]
dropEvery xs i = [ xs!!(n-1)  | n<-[1..(length xs)], n `mod` i /= 0]

-- b) guarded equations
dropEvery xs i 
    | length xs < i = xs
    | otherwise = take (i-1) xs ++ dropEvery (drop (i) xs) i

-- c) pattern matching
dropEvery [] _ = []
dropEvery [x] _ = [x]
dropEvery xs i = if length xs < i then xs else take (i-1) xs ++ dropEvery (drop (i) xs) i

-- 2. Define a polymorphic function rdups which removes duplicates from a list
rdups::Eq a => [a]->[a]
rdups (x:xs)
    | xs == [] = [x]
    | x == (head xs) = [] ++ rdups xs
    | otherwise = [x] ++ rdups xs

-- 3. Define a polymorphic function which replaces the ith element of the list (starting from zero)
replace::Int->a->[a]->[a]
replace i x xs = take i xs ++ [x] ++ drop (i+1) xs

-- 4. Define a polymorphic function slice which takes two indices, i and k, and a list, and returns the list containing the elements between the i'th and k'th element of the original list. Start counting the elements with 1
slice::Int->Int->[a]->[a]
slice i j xs = drop (i-1) (take j xs) 
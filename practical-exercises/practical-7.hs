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
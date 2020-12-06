-- 20COA108 FUNCTIONAL PROGRAMMING COURSEWORK ASSIGNMENT

-- Part 2
-- a)
makeStep::Int->Int->[Char] -- function that returns a string of length l r number of times
makeStep 0 0 = ""
makeStep _ 0 = ""
makeStep l r = ['*' | n <- [1..l]] ++ "\n" ++ makeStep l (r-1)

steps::Int->Int->Int->[Char] -- function that takes three positive Int values m n p and returns a String that can be displayed as p steps, of height m and width n, the right way up, and repeats the pattern in opposite way
steps _ _ 0 = []
steps m n p = concat ([makeStep (n * x) m | x <-[1..p]] ++ reverse [makeStep (n * x) m | x <-[1..p]])
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
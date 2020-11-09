-- PRACTICAL 4
-- 1. Given a list of lists of ints, sum the length of inner lists
sumLength::[[Int]]->Int
sumLength xs = sum[length x | x <- xs]

-- 2. Define a function that returns sum of all values of a list, but every second value is negated
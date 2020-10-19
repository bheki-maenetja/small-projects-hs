-- PRACTICAL 1
-- 1. Define a function next (using guarded equations) that takes a positive number greater than one. If it is even, divide it by two, otherwise multiply it by two and add one
next::Int->Int
next x
    | x `mod` 2 == 0 = x `quot` 2
    | otherwise = 2*x + 1

-- 2. Define a function k (using guarded equations) that takes two one of them is negative
k::Int->Int->Bool
k x y
    | x < 0 || y < 0 = True
    | otherwise = False

-- 3. Define a function absolute that returns the absolute value of its argument
absolute::Int->Int
absolute n
    | n < 0 = -n
    | otherwise = n

-- 4. Define a function trafficlights which takes a string that is the current state of trafficlights and returns the next state of the traffic lights
trafficlights::String->String
trafficlights str 
    | str == "red" = str ++ " amber"
    | str == "amber" = str ++ " green"
    | str == "green" = str ++ " red"
    | otherwise = "I don't know what you're talking about"

-- 5. Define a function summinmax3 which takes three Int values and returns the sum of the minimum and maximum integers out of these three
summinmax3::Int->Int->Int->Int
summinmax3 x y z
    | x == y && y == z = x + x
    | min x y == x && min x z == x = x + max y z
    | min y z == y && min y z == y = y + max x z
    | min z x == z && min z x == z = z + max y z


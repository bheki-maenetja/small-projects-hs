-- 20COA108 FUNCTIONAL PROGRAMMING COURSEWORK ASSIGNMENT

-- Part 2 --
-- a)
makeStep::Int->Int->[Char] -- function that returns a string of length l r number of times
makeStep 0 0 = ""
makeStep _ 0 = ""
makeStep l r = ['*' | n <- [1..l]] ++ "\n" ++ makeStep l (r-1)

steps::Int->Int->Int->[Char] -- function that takes three positive Int values m n p and returns a String that can be displayed as p steps, of height m and width n, the right way up, and repeats the pattern in opposite way
steps _ _ 0 = []
steps m n p = concat ([makeStep (n * x) m | x <-[1..p]] ++ reverse [makeStep (n * x) m | x <-[1..p]])

-- b)
makeFlag::Int->Int->[Char] -- function that makes one flag pattern
makeFlag l n
    | n == 0 = ['*' | i <-[1..l]] ++ "\n"
    | l == n = ['*' | i <-[1..l]] ++ "\n" ++ makeFlag l (n-1)
    | n == 1 = [] ++ makeFlag l (n-1)
    | otherwise = [if i == 1 || i == l || i == 1+(l-n) || i == n then '*' else ' ' | i  <-[1..l]] ++ "\n" ++ makeFlag l (n-1)

flagpattern::Int->Int->[Char] --  function flagpattern that takes two positive Int values n greater than or equal to 5, and m greater than or equal to 1, and returns a String that can be displayed as the following m 'flag' patterns of dimension n
flagpattern n m = concat [makeFlag n n | i <-[1..m]]

-- Part 3 --
removeElement::Eq a => a->[a]->[a] -- helper function to remove the first occurence on an element in a list
removeElement e xs
    | not (elem e xs) = xs
    | otherwise = takeWhile (/= e) xs ++ tail (dropWhile (/= e) xs)

eliminateCharacters::[Char]->[Char]->[Char]->([Char], [Char]) -- helper function to repeatedly cross out like characters
eliminateCharacters a [] b = (a, b)
eliminateCharacters firstWord (y:ys) xs
    | elem y firstWord || y == ' ' = eliminateCharacters (removeElement y firstWord) ys xs
    | otherwise = eliminateCharacters (removeElement y firstWord) ys (xs ++ [y])

getlphi::[Char]->[Char] -- helper function to get the correct word
getlphi str
    | strLen == 0 = " is indifferent to "
    | strLen == 1 = " loves "
    | strLen == 2 = " has physical desires for "
    | strLen == 3 = " hates "
    where
        strLen = (length str `mod` 4)

compatability::[Char]->[Char]->[Char]
compatability firstName secondName = firstName ++ getlphi (fst wordTup) ++ secondName ++ " and " ++ secondName ++ getlphi (snd wordTup) ++ firstName
    where
        wordTup = eliminateCharacters firstName secondName ""

-- Part 4 --
lengthCount::Eq a => [a]->a->Int->[Int] -- helper function that counts segments of a list
lengthCount [] _ _ = []
lengthCount (x:xs) n c
    | x /=n = lengthCount xs n (c+1)
    | length [i | i<-xs, i == n] == 0 = [c] ++ [length xs]
    | x == n = [c] ++ lengthCount xs n 0

lsplit::Eq a => [a]->a->[Int]
lsplit xs n = [i | i<-(lengthCount xs n 0), i /= 0]
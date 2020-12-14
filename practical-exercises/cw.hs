-- 20COA108 FUNCTIONAL PROGRAMMING COURSEWORK ASSIGNMENT

---- Part 1 ----
{-
a)	Among the many benefits of functional programming, there are three that 
    stand out in particular: the elimination of side effects, improved readability and easier testing

    Elimination of Side effects –-- functional programming eliminates a phenomenon known as 
                                    side effects – the unwanted tampering of global variables by a function call. 
                                    Given the fact that functional programming requires the inputs and outputs for 
                                    all functions to be clearly defined, it is nigh on impossible for a function to 
                                    mistakenly change or modify a variable that is not defined within the scope of 
                                    that function. This is a very handy feature for programmers as it enables them 
                                    to easily keep track of all variables within their program.

    Improved Readability –- in the functional programming paradigm the readability of a program is greatly increased. 
                            Given that each function in the program is pure – that is to say that its outputs are solely 
                            dependent on its inputs – programs written in the functional paradigm are easier to reason 
                            about and therefore easier for programmers to understand. Each function can be treated as a 
                            distinct unit and its functionality can be understood without having to look at any other 
                            part of the program.
    
    Easy Testing –- following on from the point about readability, programs written in the functional paradigm are 
                    also easier to test and debug. This is because, as mentioned before, every function can be 
                    treated as a distinct unit that doesn’t rely on any other part of the program. Test data can 
                    be fed to each unit and the output can be compared to a set of expected outputs.

b)  A mathematical function can be thought of as a relation between two sets of values that maps each individual 
    element of one set to a single element in the other. In simpler terms it is a mapping of an independent 
    variable to a dependent one. Haskell functions bare a few passing resemblances to mathematical functions. 
    They both take in clearly defined inputs. They are both defined over a given set of valid inputs 
    (they have a domain) and produce a particular set of valid outputs (they have a range). Additionally, for both 
    function types the output is solely dependent on the input. The behaviour of many mathematical functions can 
    be replicated in the Haskell programming language. However, whereas any given mathematical function deals with 
    values of a particular type, Haskell functions can be ‘polymorphic’ and work on a multitude of data types. In 
    that sense, Haskell functions ‘expand’ on the capabilities of mathematical functions.

c)  A higher-order function is a function that either takes one or more functions as its arguments or returns a function 
    as its output. Typical uses of higher-order functions include applying a function to all the elements of a data 
    structure (map), filtering elements from a data structure and mathematical integration. Another common use of higher-order 
    functions is the sorting of elements in a data structure such as a list. 

    For example, let’s say we wanted a function that could sort a list of names in any way we wanted to (alphabetical order, 
    reverse alphabetical order, word length, number of vowels etc.). How would we do that? We could define a function sortList 
    that takes two arguments: a list of names and a binary function (thus making sortList a higher-order function). The binary 
    function would be any function that compares two words by a given criteria and returns a Boolean value indicating whether or 
    not the first word meets the criteria more so or less so than the second word. We could then iterate over the list of names 
    two names at a time and feed each pair of names into this binary function. If the binary function returns a certain bool 
    value (e.g. True) then we would switch the places of the two names in the list otherwise we just carry on. Once the iteration 
    is complete a list of names, sorted according to a pre-defined criterion, will be returned.

REFERENCES
    * Freydenberger, D., 2020. Logic For Computer Science (Lecture Notes). Loughborough University, pp.10-87.
    * Moutafis, R., 2020. Why Developers Are Falling In Love With Functional Programming. 
    [online] Medium. Available at: <https://towardsdatascience.com/why-developers-are-falling-in-love-with-functional-programming-13514df4048e> 
    [Accessed 14 December 2020].
    * Thompson, S., 2015. Haskell: The Craft Of Functional Programming. 3rd ed. Harlow (England): Addison-Wesley.
-}

---- Part 2 ----
-- a)
steps::Int->Int->Int->[Char]
{-
This function takes 3 inputs indicating the number of steps, and the height and width of each step
It then concatenates two lists of strings with each string list being generated by the makeStep helper
function according to the three integer parameters.
-}
steps _ _ 0 = []
steps m n p = concat ([makeStep (n * x) m | x <-[1..p]] ++ reverse [makeStep (n * x) m | x <-[1..p]])

makeStep::Int->Int->[Char]
{-
This helper function takes two parameters l and r. It will return strings of length l r number of times with
each string being seperated by a "\n" character
-}
makeStep 0 0 = ""
makeStep _ 0 = ""
makeStep l r = ['*' | n <- [1..l]] ++ "\n" ++ makeStep l (r-1)

-- b)
flagpattern::Int->Int->[Char] 
{- 
This function takes two positive integer values n and m, and returns a string that can be displayed as a given flag
pattern of width n a total of m number of times
-}
flagpattern n m = concat [makeFlag n n | i <-[1..m]]

makeFlag::Int->Int->[Char] 
{- 
This helper function will return a string representing that can be displayed as a flag pattern. The flag pattern will
be of a certain size and width according to the functions two integer parameters.
-}
makeFlag l n
    | n == 0 = ['*' | i <-[1..l]] ++ "\n"
    | l == n = ['*' | i <-[1..l]] ++ "\n" ++ makeFlag l (n-1)
    | n == 1 = [] ++ makeFlag l (n-1)
    | otherwise = [if i == 1 || i == l || i == 1+(l-n) || i == n then '*' else ' ' | i  <-[1..l]] ++ "\n" ++ makeFlag l (n-1)

---- Part 3 ----
removeElement::Eq a => a->[a]->[a] 
{-
This helper function takes two parameters a value and a list, and removes the first occurence 
of that value in the list
-}
removeElement e xs
    | not (elem e xs) = xs
    | otherwise = takeWhile (/= e) xs ++ tail (dropWhile (/= e) xs)

eliminateCharacters::[Char]->[Char]->[Char]->([Char], [Char]) 
{-
This helper function will compare two strings and repeatedly eliminate characters
that exist in both strings until the strings have no characters in common
-}
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

compatibility::[Char]->[Char]->[Char]
compatibility firstName secondName = firstName ++ getlphi (fst wordTup) ++ secondName ++ " and " ++ secondName ++ getlphi (snd wordTup) ++ firstName
    where
        wordTup = eliminateCharacters firstName secondName ""

---- Part 4 ----
splitList::Eq a => a->[a]->[[a]]->[[a]] -- helper function that splits a list in segements
splitList n xs ys
    | not (elem n xs) = ys ++ [takeWhile (/= n) xs]
    | otherwise = splitList n (tail (dropWhile (/= n) xs)) (ys ++ [takeWhile (/= n) xs])

lsplit::Eq a => [a]->a->[Int]
lsplit xs n = [length i | i<-splitList n xs [], i /= []]
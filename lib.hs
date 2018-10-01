{-
MY HASKELL FUNCTION LIBRARY
    Author:
        Created: February 23, 2018
            by David Osorio
        Modified: April 02, 2018 12:11:20
            by David Osorio
    
    Organization:
        Colima Institute of Technology
    
    Career:
        Computer Engineering
    
    Subject:
        Logic and Functional Programming
-}



-------------------------------------------
--Created: February 23, 2018 09:40:17
--Modified: March 04, 2018 13:51:18

{-
 * The following three functions use GHC's type inference, though it's a good
 * practice to give them an explicit type declaration.
-}
--mul3 a = a * 3
mul3 :: Num a => a -> a
mul3 a = a * 3


--square x = x * x
square :: Num a => a -> a
square x = x * x  --It doesn't matter the parameter's name


--cube a = a ^ 3
cube :: Num x => x -> x
cube a = a ^ 3


{-
 * Solves a quadratic equation, i.e. finds the roots using the Quadratic Formula:
 * The term b^2–4ac, is called the "discriminant" because, by using its value, you can "discriminate"
 * between (that is, be able to tell the difference between) the various solution types.
 * When its value is not negative, the equation is going to have at least one (real-valued) solution;
 * if its value is not zero, the two solutions are going to be distinct.
 * Find more at: http://www.biology.arizona.edu/biomath/tutorials/Quadratic/Roots.html
 * and examples: https://www.vitutor.com/ecuaciones/2/2_e.html
-}
qeroots :: (Ord a, Floating a) => (a,a,a) -> (a,a)
qeroots (a,b,c) =
    if d < 0 then error "The equation has no real roots"
    else (x1, x2)
    where
        x1  = (-b + sqrt(d)) / (2 * a)
        x2  = (-b - sqrt(d)) / (2 * a)
        d   = b * b - 4 * a * c --Discriminant


--Basic Celsius to Fahrenheit conversion
convCF :: Fractional a => a -> a
convCF f = f * 1.8 + 32


--Basic Fahrenheit to Celsius conversion
convFC :: Fractional a => a -> a
convFC c = (c - 32) / 1.8


--Converts from Celsius to Fahrenheit and Kelvin
cfromC :: Fractional a => a -> (a,a)
cfromC c = (f,k)
    where
        f = c * 1.8 + 32
        k = c + 273.15


--Converts from Fahrenheit to Celsius and Kelvin
cfromF :: Fractional a => a -> (a,a)
cfromF f = (c,k)
    where
        c = (f - 32) / 1.8
        k = (f - 32) / 1.8 + 273.15


--Converts from Kelvin to Celsius and Fahrenheit
cfromK :: Fractional a => a -> (a,a)
cfromK k = (c,f)
    where
        c = k - 273.15
        f = (k - 273.15) * 1.8 + 32


--Converts from and to any temperature scales calling the three previous functions
--Receives a fractional-type value and a char-type degree (kind of scale) to convert from
convDeg :: (Show a, Fractional a) => a -> Char -> IO()
convDeg val deg
    | deg == 'c' = putStrLn (show val ++ " °C = " ++ show (fst (cfromC val)) ++ " °F\n" ++
        show val ++ " °C = " ++ show (snd (cfromC val)) ++ " °K") --Celsius to Fahrenheit and Kelvin
    | deg == 'f' = putStrLn (show val ++ " °F = " ++ show (fst (cfromF val)) ++ " °C\n" ++
        show val ++ " °F = " ++ show (snd (cfromF val)) ++ " °K") --Fahrenheit to Celsius and Kelvin
    | deg == 'k' = putStrLn (show val ++ " °K = " ++ show (fst (cfromK val)) ++ " °C\n" ++
        show val ++ " °K = " ++ show (snd (cfromK val)) ++ " °F") --Kelvin to Celsius and Fahrenheit


--Reverses a string using built-in function reverse
revRev :: String -> String
revRev str = reverse (str)


--Reverses a string using recursion, pattern matching and functions that operate on lists
--String is the same as [Char]
revRec :: [Char] -> [Char]
revRec []   = []
revRec str  = [last str] ++ revRec (init str)


--Reverses a string calling an auxiliary function (revIndexLoop) which acts as a for loop
revIndex :: [Char] -> [Char]
revIndex str = revIndexLoop str (length str - 1)


--Receives a string and its last index, and iterates until index 0 using recursive calls and pattern matching
--Source: https://stackoverflow.com/questions/16004365/simple-haskell-loop
--Note: This is an auxiliary function called by revIndex. Unexpected output may happen if it is run by user
revIndexLoop :: [Char] -> Int -> [Char]
revIndexLoop str n
    | n == 0    = [str !! (n)]
    | otherwise = [str !! (n)] ++ revIndexLoop str (n - 1)



-------------------------------------------
--Created: February 26, 2018 09:27:18
--Modified: March 05, 2018 19:48:44

--Pattern matching
--Another way to declare function type:
--lucky :: (Eq a, Num a) => a -> [Char]
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck, pal!"


--Pattern matching
weekday :: (Integral a) => a -> String
weekday 1 = "Sunday"
weekday 2 = "Monday"
weekday 3 = "Tuesday"
weekday 4 = "Wednesday"
weekday 5 = "Thursday"
weekday 6 = "Friday"
weekday 7 = "Saturday"
weekday x = "What you mean? A week have only 7 days"


{- --Factorial using just recursion and pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1) -}


--Factorial using recursion and guards
factR :: (Integral a) => a -> a
factR n
    | n < 0     = error "Factorial is defined only for non-negative integer numbers!"
    | n < 2     = 1
    | otherwise = n * factR (n - 1)


--Factorial using built-in function product
factP :: (Integral a) => a -> a
factP n =
    if n < 0 then error "Factorial is defined only for non-negative integer numbers!"
    else product [1..n]


{- --Prints nth Fibonacci number using just recursion and pattern matching
fibN :: Int -> Int
fibN 0 = 0
fibN 1 = 1
fibN n = fibN (n-1) + fibN (n-2) -}


--Prints nth Fibonacci number using recursion and guards
--Note: CPU can't handle more than ~35 numbers because of recursive calls
fibN :: Int -> Int
fibN n
    | n < 1     = 0
    | n == 1    = 1
    | otherwise = fibN (n-1) + fibN (n-2)


--Prints first n Fibonacci numbers using built-in function map
fibMap :: Int -> [Int]
fibMap n = map fibN [0..n]


--Prints first n Fibonacci numbers using recursion, guards and built-in function last
--Find more at: http://www.maths.surrey.ac.uk/hosted-sites/R.Knott/Fibonacci/fibtable.html
fibRec :: Int -> [Int]
fibRec n
    | n < 0     = []
    | n == 0    = [0]
    | n == 1    = [0,1]
    | otherwise = (fibRec (n-1)) ++ [ last (fibRec (n-1)) + last (fibRec (n-2)) ]


{- --If-else is an OOP approach! Use guards and where clause instead (see next function)
describeLetter :: Char -> IO()
describeLetter c =
    if c >= 'a' && c <= 'z'
        then putStrLn (show c ++ " is lowercase")
    else if c >= 'A' && c <= 'Z'
            then putStrLn (show c ++ " is uppercase")
        else putStrLn (show c ++ " is not an ASCII letter")
    --where cs = [c] --Convert char to [char], i.e. string. However, it's better to use show/read -}


--Tells if a char received is a lowercase or uppercase letter (Functional approach)
describeLetter :: Char -> IO()
describeLetter c
    | c >= 'a' && c <= 'z'  = putStrLn (show c ++ " is lowercase")
    | c >= 'A' && c <= 'Z'  = putStrLn (show c ++ " is uppercase")
    | c >= '0' && c <= '9'  = putStrLn (show c ++ " is a digit")
    | otherwise             = putStrLn (show c ++ " is not an ASCII letter")


--Computes the sum of a finite list of numbers
--Source: https://stackoverflow.com/questions/34817219/haskell-recursive-list-summation
--Note: This does not override Prelude built-in function sum
sum_ :: Num a => [a] -> a
sum_ []     = 0
sum_ (x:xs) = x + sum_ xs
--Another way:
--sum_ xs     = head xs + sum_ (tail xs)



-------------------------------------------
--Created: February 27, 2018 09:16:14
--Modified: March 03, 2018 11:10:22

--Adds an element to the beginning of a list
--Source: https://wiki.haskell.org/How_to_work_on_lists#Adding
addBeg :: t -> [t] -> [t]
addBeg a (xs) = a : xs


--Adds an element to the end of a list using recursion instead of ++ operator
--Source: https://stackoverflow.com/questions/42798257/add-a-element-at-the-end-of-list-in-haskell
addEnd :: t -> [t] -> [t]
addEnd a []     = [a]
addEnd a (x:xs) = x : addEnd a xs


--Gets the middle index of a list
getMiddleIndex :: [t] -> Int
getMiddleIndex (xs) --List
    | mod (length xs) 2 == 0    = middle --Even length
    | otherwise                 = middle - 1 --Odd length (Haskell takes just middle value as its default value)
    where middle = div (length xs) 2


--Inserts an element at a specific index (position) of a list
--This works for strings and any kind of numeric list
--Find more at: https://stackoverflow.com/questions/19170483/inserting-an-integer-into-a-list-at-specific-place
insertAt :: a -> [a] -> Int -> [a]
insertAt elem lis pos = (take pos lis) ++ [elem] ++ (drop pos lis)


--Inserts an element into the middle of a list, calling insertAt and getMiddleIndex functions
insertAtMiddle :: a -> [a] -> [a]
insertAtMiddle elem lis = insertAt elem lis (getMiddleIndex (lis))



-------------------------------------------
--Created: March 02, 2018 09:37:39
--Modified: March 04, 2018 21:40:07

{-
 * List comprehension:
 * Note that weeding out lists by predicates is also called FILTERING. We take a list of numbers and
 * we filter them by the PREDICATE. Now for another example. Let's say we want a comprehension that
 * replaces each odd number greater than 10 with "BANG!" and each odd number that's less than 10 with "BOOM!".
 * If a number isn't odd, we throw it out of our list. For convenience, we'll put that comprehension
 * inside a function so we can easily reuse it.
 * Find more at: http://learnyouahaskell.com/starting-out#im-a-list-comprehension
-}
boomBang :: (Integral a) => [a] -> [[Char]]
boomBang xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x ]



-------------------------------------------
--Created: March 05, 2018 09:07:07
--Modified: March 10, 2018 10:43:51

--Shifts elements to the left by one position in a list, e.g. [1,2,3,4] = [2,3,4,1]
shiftLeft :: [t] -> [t]
shiftLeft [] = []
shiftLeft xs = tail xs ++ [head xs]
--Another way:
--shiftLeft xs = drop 1 xs ++ [head xs]


--Shifts elements to the right by one position in a list, e.g. [1,2,3,4] = [4,1,2,3]
shiftRight :: [t] -> [t]
shiftRight [] = []
shiftRight xs = last xs : init xs
--Another way:
--shiftRight xs = drop (l - 1) xs ++ take (l - 1) xs
    --where l = length xs


{- For example xs = [1,2,3,4]
    Shift Left      Shift Right
n=1
    [2,3,4,1]       [4,1,2,3]
n=2
    [3,4,1,2]       [3,4,1,2]
n=3
    [4,1,2,3]       [2,3,4,1]
n=4
    [1,2,3,4]       [1,2,3,4]
n=5 = n=1
    [2,3,4,1]       [4,1,2,3]
n=6 = n=2
    [3,4,1,2]       [3,4,1,2]
n=7 = n=3
    [4,1,2,3]       [2,3,4,1]
n=8 = n=4
    [1,2,3,4]       [1,2,3,4]
n=12 = n=4
    [1,2,3,4]       [1,2,3,4]
n=15 = n=3
    [4,1,2,3]       [2,3,4,1]
n=16 = n=4
    [1,2,3,4]       [1,2,3,4]
Conclusion: Use mod operator
-}


--Shifts elements to the left or right (to) by n positions (n)
shiftTo :: Eq a => Char -> [a] -> Int -> [a]
shiftTo to xs n
    | xs == []                                  = []
    | n < 0                                     = error "An index must be a non-negative integer"
    | (xs /= [] && (to == 'l' || to == 'L'))    = drop n_ xs ++ take n_ xs
    | (xs /= [] && (to == 'r' || to == 'R'))    = drop (length xs - n_) xs ++ take (length xs - n_) xs
    | otherwise                                 = error "Shift operation is just to the left ('l') or right ('r')"
    where n_ = mod n (length xs)



-------------------------------------------
--Created: March 06, 2018 09:33:19
--Modified: March 10, 2018 12:20:48

{-
 * Removes first occurrence of a value from a list using guards and built-in function takeWhile, e.g.
 * xs = [1,2,3,4,5,6,4] --> removeFirst xs 4 --> [1,2,3,5,6,4]
 * This works for strings and any kind of numeric list
-}
removeFirst :: Eq a => [a]-> a -> [a]
removeFirst xs el
    | xs == []              = []
    | elem el xs == False   = xs --error "List does not contain element"
    | otherwise             = tw ++ drop ((length tw) + 1) xs
    where tw = takeWhile (/=el) xs


--Removes all ocurrences of a value from a list using list comprehension
removeAll :: Eq a => [a]-> a -> [a]
removeAll xs el = [ x | x <- xs, x/=el ]


--Because strings are lists, we can use list comprehensions to process and produce strings
--Here's a function that takes a string and removes everything except uppercase letters from it
removeNonUppercase :: [Char] -> [Char]
removeNonUppercase str = [ c | c <- str, elem c ['A'..'Z'] ]


--Removes all whitespaces from a string
removeAllSpaces :: [Char] -> [Char]
removeAllSpaces str = [ char | char <- str, char/=' ' ]


--Removes whitespaces from a string and uses built-in function reverse to check if it is a palindrome
isPalindrome :: [Char] -> Bool
isPalindrome str = trimmed == reverse trimmed
    where trimmed = removeAllSpaces str


--Tells whether a string is a palindrome calling isPalindrome function
palindrome :: [Char] -> IO()
palindrome str
    | exp == True   = putStrLn ("'" ++ str ++ "' is a palindrome")
    | exp == False  = putStrLn ("'" ++ str ++ "' is not a palindrome")
    where exp = isPalindrome str



-------------------------------------------
--Created: March 10, 2018 12:25:28
--Modified: March 30, 2018 21:51:27

{-
 * Finds all factors of a natural number:
 * A number's factors are numbers which multiply together to form it as a product.
 * Another way of thinking of this is that every number is the product of multiple factors.
-}
factors :: Integral a => a -> [a]
factors n = [ x | x <- [1..n], mod n x == 0 ]


{-
 * Prints all perfect numbers less than n, (LT stands for Less Than):
 * In number theory, a perfect number is a positive integer that is equal to the sum of its proper positive
 * divisors, that is, the sum of its positive divisors excluding the number itself (also known as its aliquot sum).
 * It is not known whether there are any odd perfect numbers, nor whether infinitely many perfect numbers exist.
 * The first perfect number is 6. Its proper divisors are 1, 2, and 3, and 1 + 2 + 3 = 6. Equivalently,
 * the number 6 is equal to half the sum of all its positive divisors: ( 1 + 2 + 3 + 6 ) / 2 = 6.
 * The next perfect number is 28 = 1 + 2 + 4 + 7 + 14. This is followed by the perfect numbers 496 and 8128.
 * Note: List comprehension approach is very slow since Haskell must obtain all factors for every number
 * less than n. If the list used is [1..n], calling perfectNumbers 8200 needs about a minute to show the
 * first four perfect numbers. If the list used is [2,4..n], the time needed decreases by half.
-}
perfectNumbersLT :: Integral a => a -> [a]
perfectNumbersLT n = [ x | x <- [2,4..n], sum (init (factors x)) == x ]


{-
 * Prints first n perfect numbers, (HM stands for How Many):
 * Functionality.
 * This functions calls perfectNumbersGenerator which uses a formula to obtain the numbers faster than the
 * previous list comprehension. Calling perfectNumbersHM 5 needs about 90 seconds. It hangs after 6 numbers.
 * Description.
 * Perfect numbers follow a PATTERN: There are all of the powers of 2 from 1 up to a certain number,
 * and then a prime number that is equal to DOUBLE the last power of two, minus 1 [..] The rest of the factors
 * are each power of two TIMES that prime number. So, our first five perfect numbers are:
 * 2 * 3 = 6
 * 4 * 7 = 28
 * 16 * 31 = 496
 * 64 * 127 = 8,128
 * 4096 * 8191 = 33,550,336
 * The pattern (Power of Two) * (Double that Power - 1) turns into this formula:
 * Perfect Number = 2^(n - 1) * (2^n - 1) where (2^n - 1) is a Mersenne prime.
 * Find more about this explanation at: http://mathforum.org/dr.math/faq/faq.perfect.html
 * List of perfect numbers at: https://en.wikipedia.org/wiki/List_of_perfect_numbers
 * List of known Mersenne prime numbers: https://www.mersenne.org/primes
-}
perfectNumbersHM :: (Eq t, Num t, Integral a) => t -> [a]
perfectNumbersHM n = perfectNumbersGenerator n 2


{-
 * Finds first n perfect numbers (howManyMore) using powers of 2 (n) from 2 until the desired amount
 * of numbers is found (i.e. until howManyMore == 0 condition is met)
 * Note: This is an auxiliary function called by perfectNumbersHM. Unexpected output may happen if it is run
 * by user
-}
perfectNumbersGenerator :: (Eq t, Num t, Integral a1, Integral a2) => t -> a2 -> [a1]
perfectNumbersGenerator howManyMore n
    | howManyMore == 0                  = []
    | pnf == sum (init (factors pnf))   = pnf : perfectNumbersGenerator (howManyMore - 1) (n + 1)
    | otherwise                         = perfectNumbersGenerator howManyMore (n + 1)
    where pnf = 2^(n - 1) * (2^n - 1) --Perfect Number Formula


--Prints first n Mersenne primes using recursion and guards
--In mathematics, a Mersenne prime is a prime number that is one less than a power of two
mersennePrimes :: (Num a, Integral b) => b -> [a]
mersennePrimes n
    | n < 1     = []
    | n == 1    = [0]
    | otherwise = mersennePrimes (n - 1) ++ [ (2^(n - 1) - 1) ]


{-
 * Prints all triples ('ternas' in Spanish) between a given range:
 * Tuple introduction.
 * In mathematics, a tuple is a finite ordered list (sequence) of elements. An n-tuple is a sequence
 * (or ordered list) of n elements, where n is a non-negative integer. There is only one 0-tuple,
 * an empty sequence, or empty tuple, as it is referred to. An n-tuple is defined inductively using the
 * construction of an ordered pair. Relational databases may formally identify their rows (records) as tuples.
 * Etymology.
 * The term originated as an abstraction of the sequence: single, double, triple, quadruple, quintuple,
 * sextuple, septuple, octuple, ..., n‑tuple, ..., where the prefixes are taken from the Latin names of the
 * numerals. The unique 0‑tuple is called the null tuple. A 1‑tuple is called a singleton, a 2‑tuple
 * is called an ordered pair and a 3‑tuple is a TRIPLE or TRIPLET. n can be any non-negative integer.
 * Properties.
 * A tuple has properties that distinguish it from a SET.
 * Find them at: https://en.wikipedia.org/wiki/Tuple#Properties
-}
triples :: (Num a, Enum a) => a -> [(a,a,a)]
triples n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n] ]


{-
 * Prints all Pythagorean triples ('ternas Pitagóricas' in Spanish) between a given range:
 * Definition.
 * A Pythagorean triple consists of three positive integers a, b, and c, such that a^2 + b^2 = c^2.
 * Such a triple is commonly written (a, b, c), and a well-known example is (3, 4, 5). If (a, b, c)
 * is a Pythagorean triple, then so is (ka, kb, kc) for any positive integer k. A primitive Pythagorean triple
 * is one in which a, b and c are coprime (that is, they have no common divisor larger than 1). A triangle
 * whose sides form a Pythagorean triple is called a Pythagorean triangle, and is necessarily a right triangle.
 * Examples.
 * There are 16 PRIMITIVE Pythagorean triples with c ≤ 100:
 * Find them at: https://en.wikipedia.org/wiki/Pythagorean_triple#Examples
 * Note, for example, that (6, 8, 10) is not a primitive Pythagorean triple, as it is a multiple of (3, 4, 5).
 * Note: This list comprehension prints all primitive and non-primitive triples.
-}
pythTriples :: (Num a, Eq a, Enum a) => a -> [(a,a,a)]
pythTriples n = [ (x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2 ]


{-
 * Prints all prime numbers less than or equal to n, calling factors function as part of the predicate:
 * A prime number is a positive integer that cannot be made by multiplying other positive integers,
 * therefore it has two factors: 1 and the number itself.
 * List of first 1000 primes: https://primes.utm.edu/lists/small/1000.txt
-}
primes :: Integral a => a -> [a]
primes n = [ x | x <- [2..n], length (factors x) == 2 ]
--Another way:
--primes n = [ x | x <- [2..n], factors x == [1,x] ]


{-
 * Takes two integers and prints Greatest Common Divisor (gcd or 'Máximo Común Divisor (mcd)' in Spanish):
 * The calculation uses Euclid's algorithm rather than prime factorizations,
 * therefore this is not a Functional approach.
 * Note: This does not override Prelude built-in function gcd.
-}
gcd_ :: Integral a => a -> a -> a
gcd_ a b
    | b == 0    = a
    | otherwise = gcd_ b (mod a b)


{-
 * Takes two integers and prints Least common multiple (lcm or 'Mínimo Común Múltiplo (mcm)' in Spanish):
 * The calculation uses Reduction by the greatest common divisor rather than prime factorizations and
 * unique factorization theorem, therefore this is not a Functional approach.
 * Note: This does not override Prelude built-in function lcm.
-}
lcm_ :: Integral a => a -> a -> a
lcm_ a b = div (a * b) (gcd_ a b)


{- Find more about gcd and lcm at: https://www.geogebra.org/m/qaxeCpYb (factorization)
and: http://programmertech.com/program/csharp/csharp-program-find-lcm-of-numbers (table method) -}



-------------------------------------------
--Created: March 13, 2018 09:35:19
--Modified: March 31, 2018 09:34:08

{-
 * Merges two ascending-sorted lists, either strings or numeric lists, using pattern matching and recursion, e.g.
 * merge "abcd" "ABCD" --> "ABCDabcd"
 * merge [-1,5,10,15] [-10,0,2,4,6] --> [-10,-1,0,2,4,5,6,10,15]
 * Source: https://stackoverflow.com/questions/3938438/merging-two-lists-in-haskell
-}
merge :: Ord a => [a] -> [a] -> [a]
merge [] []             = []
merge (x:xs) []         = x:xs
merge [] (y:ys)         = y:ys
--merge xs []             = xs --Missing head causes a skipped number
--merge [] ys             = ys --Missing head causes a skipped number
merge (x:xs) (y:ys)     =
    if x < y then x : merge xs (y:ys)
    else y : merge (x:xs) ys



-------------------------------------------
--Created: March 15, 2018 09:31:01
--Modified: March 31, 2018 23:14:55

{-
 * TODO LIST: SHELL SORT & RADIX ALGORITHMS
 * REFERENCES:
 * http://bigocheatsheet.com (Big-O Complexity Chart)
 * http://learnyouahaskell.com/recursion#quick-sort
 * https://www.geeksforgeeks.org/quick-sort
 * https://www.geeksforgeeks.org/shellsort
 * https://en.wikipedia.org/wiki/Radix_sort
 * https://smthngsmwhr.wordpress.com/2012/11/09/sorting-algorithms-in-haskell (Quicksort, Mergesort and Bubble)
-}


--Bubble sort algorithm
--Source: https://gist.github.com/weefbellington/b924220c93fde76fcb27
--Note: It is too slow and impractical, calling bubbleSort [100,99..1] needs several minutes. It hangs, in fact
bubbleSort :: Ord a => [a] -> [a]
bubbleSort []           = []
bubbleSort [x]          = [x]
bubbleSort (x:y:rest)   = bubbleSort (init bubbled) ++ [last bubbled]
    where
        (first, second) = if x < y then (x,y) else (y,x)
        bubbled = first : bubbleSort (second:rest)


--Insertion sort algorithm
--Source: https://stackoverflow.com/questions/28550361/insertion-sort-in-haskell
--Note: This is a more practical method than bubble, calling insertionSort [5000,4999..1] needs about 15 seconds
insertionSort :: Ord a => [a] -> [a]
insertionSort [x]       = [x]
insertionSort (x:xs)    = insertAsc x (insertionSort xs)


--Inserts an element into a list in ascending order, i.e. in such a way that elem
--is bigger than those elements before it and smaller than or equal to the elements that follow it
insertAsc :: Ord a => a -> [a] -> [a]
insertAsc elem []       = [elem]
insertAsc elem (x:xs)   =
    if elem < x then elem : (x:xs)
    else x : insertAsc elem xs


--Quicksort algorithm
--Pivot chosen: leftmost element of the partition (list head)
--Authors: Williams Estrada & Ricardo Villagrana
quickSort :: Ord a => [a] -> [a]
quickSort []            = []
quickSort (x:xs)        =
    quickSort (filter (<=x) xs) ++ [x] ++ quickSort (filter (>x) xs)


{- --USING "MEDIAN-OF-THREE" AS A WAY TO FIND THE PIVOT DOES NOT WORK AS EXPECTED. THE FOLLOWING CODE IS WRONG:
quickSort []        = []
quickSort xs        =
    quickSort (filter (<=piv) xs) ++ [piv] ++ quickSort (filter (>piv) xs)
        where piv = quickSortMedianOfThree xs


--Pivot chosen: median of the first, middle and last element of the partition
--Source: https://stackoverflow.com/questions/23158640/quick-sort-algorithm-median-of-three
quickSortMedianOfThree (x:xs)
    | length (x:xs) < 3 = x --If the list has one or two elements, the head becomes the pivot
    | otherwise         = insertionSort ([x] ++ [(x:xs) !! getMiddleIndex (x:xs)] ++ [last (x:xs)]) !! 1 -}
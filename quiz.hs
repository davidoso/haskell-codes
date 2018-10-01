{-
HASKELL QUIZ, UNIT II
    Author:
        Created: May 29, 2018
            by David Osorio
        Modified: June 01, 2018 17:18:33
            by David Osorio
    
    Organization:
        Colima Institute of Technology
    
    Career:
        Computer Engineering
    
    Subject:
        Logic and Functional Programming
-}



-------------------------------------------
--Created: May 29, 2018 09:27:33
--Modified: June 01, 2018 17:18:29


--AUXILIARY FUNCTIONS

--Factorial using built-in function product
factP :: (Integral a) => a -> a
factP n =
    if n < 0 then error "Factorial is defined only for non-negative integer numbers!"
    else product [1..n]


{-
 * Tells whether three given sides can make up a triangle:
 * In mathematics, the triangle inequality theorem states that for any triangle, the sum of the lengths
 * of any two sides must be greater than the length of the remaining side.
-}
sidesFormATriangle :: (Ord a, Num a) => a -> a -> a -> Bool
sidesFormATriangle a b c = (a + b) > c && (a + c) > b && (b + c) > a


--Returns true if the given year is a leap year. It calls isDivisible function (exercise 5)
isLeapYear :: Integral a => a -> Bool
isLeapYear y
    | yearIsDivisibleBy 400     = True
    | yearIsDivisibleBy 100     = False
    | yearIsDivisibleBy 4       = True
    | otherwise                 = False
    where
        yearIsDivisibleBy n     = isDivisible y n == True


--8 QUIZ EXERCISES

--EXERCISE 1
{-
 * Prints the binomial coefficient [(n k) or nCK] for non-negative n and k:
 * Description.
 * The binomial coefficient '(n k)' or 'nCk' is the number of ways of picking k unordered outcomes from
 * n possibilities, also known as a combination or combinatorial number.
 * Commonly, a binomial coefficient is indexed by a pair of integers n ≥ k ≥ 0.
 * nCk therefore gives the number of k-subsets possible out of a set of n distinct items.
 * The binomial coefficients occur in many areas of mathematics, especially combinatorics. The symbol nCk
 * is usually read as "n choose k" because there are nCk ways to choose an (unordered) subset of k elements from
 * a fixed set of n elements. For example, there are (4 2) ways to choose 2 elements from {a, b, c, d}, namely
 * {a, b}, {a, c}, {a, d}, {b, c}, {b, d}, {c, d}.
 * nCk is also the coefficient of the x^k term in the polynomial expansion of the binomial power (1 + x)^n
 * (see exercise 8)
 * Find more at: https://en.wikipedia.org/wiki/Binomial_coefficient
-}
comb :: Integral a => a -> a -> a
comb n k = div (factP n) (factP k * factP (n - k))


--EXERCISE 2
{-
 * Calculates the area of a triangle using Heron's formula:
 * Description.
 * In geometry, Heron's formula, named after Hero of Alexandria (10-70 AD), gives the area of a triangle by
 * requiring no arbitrary choice of side as base or vertex as origin, contrary to other formulas for the area
 * of a triangle, such as half the base times the height or half the norm of a cross product of two sides.
 * Find a calculator at: https://www.mathsisfun.com/geometry/herons-formula.html
 * and examples: https://en.wikipedia.org/wiki/Heronian_triangle#Examples
-}
heron :: (Ord p, Floating p) => p -> p -> p -> p
heron a b c
    | (sidesFormATriangle a b c) == False   = error "The three sides do not form a triangle"
    | otherwise                             = sqrt(sp * (sp - a) * (sp - b) * (sp - c))
    where sp = (a + b + c) / 2 --Semiperimeter


--EXERCISE 3
{-
 * The following three functions redefine map function:
 * In many programming languages, including Haskell, map is the name of a higher-order function that applies a
 * given function to each element of a list, returning a list of results in the same order.
 * References:
    http://www.glc.us.es/~jalonso/vestigium/i1m2016-funciones-de-orden-superior-en-haskell
    https://stackoverflow.com/questions/5726445/how-would-you-define-map-and-filter-using-foldr-in-haskell
    https://stackoverflow.com/questions/1757740/how-does-foldr-work
    http://learnyouahaskell.com/higher-order-functions#maps-and-filters
 * Note: None of these functions override Prelude built-in function map
-}
--Redefines map function using foldr
map_f :: Foldable t1 => (t2 -> a) -> t1 t2 -> [a]
map_f f = foldr (\x xs -> f x : xs) []


--Redefines map function using list comprehension
map_l :: (t -> a) -> [t] -> [a]
map_l f xs = [f x | x <- xs]


--Redefines map function using recursion
map_r :: (t -> a) -> [t] -> [a]
map_r _ []      = []
map_r f (x:xs)  = f x : map_r f xs


--EXERCISE 4
{-
 * The following three functions redefine filter function:
 * filter is a function that takes a predicate (a predicate is a function that tells whether something
 * is true or not, i.e. a function that returns a boolean value) and a list and then returns the list
 * of elements that satisfy the predicate.
 * References:
    http://www.glc.us.es/~jalonso/vestigium/i1m2016-funciones-de-orden-superior-en-haskell
    https://stackoverflow.com/questions/5726445/how-would-you-define-map-and-filter-using-foldr-in-haskell
    http://learnyouahaskell.com/higher-order-functions#maps-and-filters
 * Note: Just as exercise 3, none of these functions override Prelude built-in function filter
-}
--Redefines filter function using foldr
filter_f :: Foldable t => (a -> Bool) -> t a -> [a]
filter_f p = foldr (\x xs -> if p x then x : xs else xs) []


--Redefines filter function using list comprehension
filter_l :: (a -> Bool) -> [a] -> [a]
filter_l f xs = [x | x <- xs, f x]


--Redefines filter function using recursion
filter_r :: (a -> Bool) -> [a] -> [a]
filter_r _ []               = []
filter_r f (x:xs) | f x     = x : filter_r f xs
    | otherwise             = filter_r f xs


--EXERCISE 5
--Returns true if an integer x is divisible by an integer y
isDivisible :: Integral a => a -> a -> Bool
isDivisible x y
    | y == 0        = error "Division by 0. 2nd number must be > 0"
    | rem x y == 0  = True --For rem operator, unlike mod, sign of the result is the same as sign of x
    | otherwise     = False


--EXERCISE 6
{-
 * Prints the day of the week of a given date in the format dd mm yyyy:
 * Formula.
 * The main formula and explanation is found at:
 * https://es.wikibooks.org/wiki/Algoritmia/Algoritmo_para_calcular_el_d%C3%ADa_de_la_semana
 * For simplicity's sake, the formula was split into four parts (weekdayPart1.. weekdayPart4).
 * More references:
    https://www.rosettacode.org/wiki/Day_of_the_week
    https://en.wikipedia.org/wiki/Determination_of_the_day_of_the_week
    https://en.wikipedia.org/wiki/Zeller%27s_congruence
    https://en.wikipedia.org/wiki/Doomsday_rule
    https://code.msdn.microsoft.com/windowsapps/Congruencia-de-Zeller-48268c0a
    https://www.timeanddate.com/date/doomsday-weekday.html
    https://github.com/TheBB/doomsday/blob/master/Main.hs
    https://github.com/neunenak/Doomsday/blob/master/doomsdayAlgorithm.hs
-}
getWeekday :: Integral a => a -> a -> a -> [Char]
getWeekday d m y
    | d < 1 || d > 31       = error "Invalid date! Day (first number) must be >= 1 or <= 31"
    | m < 1 || m > 12       = error "Invalid date! Month (second number) must be >= 1 or <= 12"
    | y < 1900 || y > 2099  = error "Invalid date! Year (third number) must be >= 1900 or <= 2099"
    | validDay == False     = error "Invalid date! The specified day is not in the month"
    | otherwise             =
        weekdayName (rem (weekdayPart1 y + weekdayPart2 y + weekdayPart3 m y + weekdayPart4 d) 7)
    where
        validDay = dayIsInTheMonth d m (isLeapYear y) == True


--Returns true if a given day is within a month in the format dd mm [true/false if the year is a leap year]
--Note: This is an auxiliary function called by getWeekday. Unexpected output may happen if it is run by user
dayIsInTheMonth :: (Eq a1, Eq a2, Num a1, Num a2) => a1 -> a2 -> Bool -> Bool
dayIsInTheMonth 29 2 True   = True  --If the year is a leap year, then February 29 exists
dayIsInTheMonth 29 2 False  = False --If the year is a normal year, then February 29 doesn't exist
dayIsInTheMonth 30 2 _      = False --February 30 and 31, April 31, and so on don't exist
dayIsInTheMonth 31 2 _      = False
dayIsInTheMonth 31 4 _      = False
dayIsInTheMonth 31 6 _      = False
dayIsInTheMonth 31 9 _      = False
dayIsInTheMonth 31 11 _     = False
dayIsInTheMonth _ _ _       = True  --Any other date is valid


--Prints the days of the week, starting on Sunday
--Note: This is an auxiliary function called by getWeekday. Unexpected output may happen if it is run by user
weekdayName :: (Eq a, Num a) => a -> [Char]
weekdayName 0 = "Sunday"
weekdayName 1 = "Monday"
weekdayName 2 = "Tuesday"
weekdayName 3 = "Wednesday"
weekdayName 4 = "Thursday"
weekdayName 5 = "Friday"
weekdayName 6 = "Saturday"
weekdayName x = "Error! This pattern shouldn't have been reached"


--Note: This is an auxiliary function called by getWeekday. Unexpected output may happen if it is run by user
weekdayPart1 :: Integral a => a -> a
weekdayPart1 y = rem (y - 1) 7


--Note: This is an auxiliary function called by getWeekday. Unexpected output may happen if it is run by user
weekdayPart2 :: Integral a => a -> a
weekdayPart2 y = rem (div (y - 1) 4 - 3 * div (div (y - 1) 100 + 1) 4) 7


--Note: This is an auxiliary function called by getWeekday. Unexpected output may happen if it is run by user
weekdayPart3 :: (Integral a1, Integral a2) => a2 -> a1 -> a2
weekdayPart3 m y =
    if isLeapYear y == True then weekdayPart3LeapYear m
    else weekdayPart3NormalYear m


--Note: This is an auxiliary function called by weekdayPart3. Unexpected output may happen if it is run by user
weekdayPart3LeapYear :: (Integral a) => a -> a
weekdayPart3LeapYear 1  = 0
weekdayPart3LeapYear 2  = 3
weekdayPart3LeapYear 3  = 4
weekdayPart3LeapYear 4  = 0
weekdayPart3LeapYear 5  = 2
weekdayPart3LeapYear 6  = 5
weekdayPart3LeapYear 7  = 0
weekdayPart3LeapYear 8  = 3
weekdayPart3LeapYear 9  = 6
weekdayPart3LeapYear 10 = 1
weekdayPart3LeapYear 11 = 4
weekdayPart3LeapYear 12 = 6
weekdayPart3LeapYear x  = -1 --This pattern should neber be reached


--Note: This is an auxiliary function called by weekdayPart3. Unexpected output may happen if it is run by user
weekdayPart3NormalYear :: (Integral a) => a -> a
weekdayPart3NormalYear 1    = 0
weekdayPart3NormalYear 2    = 3
weekdayPart3NormalYear 3    = 3
weekdayPart3NormalYear 4    = 6
weekdayPart3NormalYear 5    = 1
weekdayPart3NormalYear 6    = 4
weekdayPart3NormalYear 7    = 6
weekdayPart3NormalYear 8    = 2
weekdayPart3NormalYear 9    = 5
weekdayPart3NormalYear 10   = 0
weekdayPart3NormalYear 11   = 3
weekdayPart3NormalYear 12   = 5
weekdayPart3NormalYear x    = -1 --This pattern should neber be reached


--Note: This is an auxiliary function called by getWeekday. Unexpected output may happen if it is run by user
weekdayPart4 :: Integral a => a -> a
weekdayPart4 d = rem d 7


--EXERCISE 7
{-
 * Prints an integer as a string:
 * Members of Show can be presented as strings. The most used function that deals with the Show typeclass is show.
 * It takes a value whose type is a member of Show and prints it out to the terminal as a string.
 * Find more at: http://learnyouahaskell.com/types-and-typeclasses#typeclasses-101
-}
--Another way to declare function type (accepts any number or char):
--int2str :: (Show a) => a -> String
int2str :: (Show a, Integral a) => a -> String
int2str a = show a


--Splits a positive integer into individual digits and returns a list
--Is it possible to take the list and make up the number again? D:
digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (div n 10) ++ [rem n 10]


--EXERCISE 8
--Prints nth row of Pascal's triangle
pascal :: Integral a => a -> [a]
pascal n
    | n < 0     = error "Row index must be >= 0"
    | n == 0    = [1]
    | otherwise = [ comb n x | x <- [0..n] ]
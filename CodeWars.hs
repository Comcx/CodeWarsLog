

import Data.List
import Data.Char
import Control.Lens
import Text.Printf(printf)



{-Description:
In a small town the population is p0 = 1000 at the beginning of a year. 
The population regularly increases by 2 percent per year and 
moreover 50 new inhabitants per year come to live in the town.
How many years does the town need to see its population greater or equal to p = 1200 inhabitants?

At the end of the first year there will be: 
1000 + 1000 * 0.02 + 50 => 1070 inhabitants

At the end of the 2nd year there will be: 
1070 + 1070 * 0.02 + 50 => 1141 inhabitants (number of inhabitants is an integer)

At the end of the 3rd year there will be:
1141 + 1141 * 0.02 + 50 => 1213

It will need 3 entire years.
More generally given parameters:
p0, percent, aug (inhabitants coming or leaving each year), p (population to surpass)

the function nb_year should return n number of entire years needed to 
get a population greater or equal to p.

aug is an integer, percent a positive or null number, p0 and p are positive integers (> 0)

Examples:
nb_year(1500, 5, 100, 5000) -> 15
nb_year(1500000, 2.5, 10000, 2000000) -> 10
Note: Don't forget to convert the percent parameter as a percentage in the body of your function: 
if the parameter percent is 2 you have to convert it to 0.02.-}


nbYear :: Int -> Double -> Int -> Int -> Int
nbYear p0 percent aug p
  = length
  $ takeWhile (< p)
  $ iterate ((+ aug ) . floor . (* (1+percent/100)) . fromIntegral) p0







{-Description:
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
The sum of these multiples is 23.

Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in.

Note: If the number is a multiple of both 3 and 5, only count it once.-}



solution :: Integer -> Integer
solution number = sum [n | n <- [1..number - 1], n `mod` 3 == 0 || n `mod` 5 == 0]

----------------------------------------------------------------



solution' :: Integer -> Integer
solution' n = sum $ nub $ [3,6..n-1] ++ [5,10..n-1]







{-Description:
Your goal in this kata is to implement a difference function, which subtracts one list from another and returns the result.

It should remove all values from list a, which are present in list b.

difference [1,2] [1] == [2]
If a value is present in b, all of its occurrences must be removed from the other:

difference [1,2,2,2,3] [2] == [1,3]-}


difference :: Eq a => [a] -> [a] -> [a]
difference a b = filter (`notElem` b) a







{-Description:
You are given an array strarr of strings and an integer k. Your task is to return the first longest string consisting of k consecutive strings taken in the array.

#Example: longest_consec(["zone", "abigail", "theta", "form", "libe", "zas", "theta", "abigail"], 2) --> "abigailtheta"

n being the length of the string array, if n = 0 or k > n or k <= 0 return "".-}


longestConsec :: [String] -> Int -> String
longestConsec strarr k | k <= 0      = ""
                       | preLen == 0 = ""
                       | k > preLen  = ""
                       | otherwise   = (\(Just a) -> a) (find (\x -> length x == maxLen) hub)
  where hub    = map (\x -> concat $ take k (drop x strarr)) [0 .. (preLen - k)]
        maxLen = maximum $ map length hub
        preLen = length strarr





{-Description:
You have an array of numbers.
Your task is to sort ascending odd numbers but even numbers must be on their places.

Zero isn't an odd number and you don't need to move it. If you have an empty array, you need to return it.

Example

sortArray [5, 3, 2, 8, 1, 4] == [1, 3, 2, 8, 5, 4]-}


sortArray :: [Int] -> [Int]
sortArray = replaceOdd <$> id <*> sort . filter odd
  where replaceOdd xs [] = xs
        replaceOdd (x:xs) oos@(o:os)
          | even x    = x : replaceOdd xs oos
          | otherwise = o : replaceOdd xs os

---------------------------------------------------------------
sortArray' :: [Int] -> [Int]
sortArray' xs = xs & partsOf (each . filtered odd) %~ sort






{-Description:
Your task is to make a function that can take any non-negative integer as a argument and return it with its digits in descending order. Essentially, rearrange the digits to create the highest possible number.

Examples:
Input: 21445 Output: 54421

Input: 145263 Output: 654321

Input: 1254859723 Output: 9875543221-}

descendingOrder :: Integer -> Integer
descendingOrder n = toNum (sort $ toArr n) 0
  where toArr num | num < 10  = [num]
                  | otherwise = (mod num 10) : toArr (div num 10)
        toNum [] q     = 0
        toNum (x:xs) q = x * 10 ^ q + toNum xs (q+1)

------------------------------------------------------------------
descendingOrder' :: Integer -> Integer
descendingOrder' = read . reverse . sort . show





{-Description:
Well met with Fibonacci bigger brother, AKA Tribonacci.

As the name may already reveal, it works basically like a Fibonacci, 
but summing the last 3 (instead of 2) numbers of the sequence to generate the next. 
And, worse part of it, regrettably I won't get to 
hear non-native Italian speakers trying to pronounce it :(

So, if we are to start our Tribonacci sequence with [1, 1, 1] as a starting input (AKA signature), 
we have this sequence:

[1, 1 ,1, 3, 5, 9, 17, 31, ...]
But what if we started with [0, 0, 1] as a signature? 
As starting with [0, 1] instead of [1, 1] basically shifts the common Fibonacci sequence by once place, 
you may be tempted to think that we would get the same sequence shifted by 2 places, 
but that is not the case and we would get:

[0, 0, 1, 1, 2, 4, 7, 13, 24, ...]
Well, you may have guessed it by now, 
but to be clear: you need to create a fibonacci function that given a signature array/list, 
returns the first n elements - signature included of the so seeded sequence.

Signature will always contain 3 numbers; n will always be a non-negative number; 
if n == 0, then return an empty array and be ready for anything else which is not clearly specified ;)

If you enjoyed this kata more advanced and generalized version of it can be found in the Xbonacci kata

[Personal thanks to Professor Jim Fowler on Coursera for his awesome classes that 
I really recommend to any math enthusiast and for 
showing me this mathematical curiosity too with his usual contagious passion :)]-}


tribonacci :: Num a => (a, a, a) -> Int -> [a]
tribonacci (a, b, c) n = take n (infArr (a, b, c))
  where infArr (a, b, c) = [a, b, c] ++ infArr (a+b+c, a+2*b+2*c, 2*a+3*b+4*c)

---------------------------------------------------------------------------------

tribonacci' :: Num a => (a, a, a) -> Int -> [a]
tribonacci' _ n | n < 1 = []
tribonacci' (a, b, c) n = a : tribonacci (b, c, a+b+c) (n-1)










{-A pangram is a sentence that contains every single letter of the alphabet at least once. 
For example, the sentence "The quick brown fox jumps over the lazy dog" is a pangram, 
because it uses the letters A-Z at least once (case is irrelevant).

Given a string, detect whether or not it is a pangram. 
Return True if it is, False if not. Ignore numbers and punctuation.-}

isPangram :: String -> Bool
isPangram str = length (nub $ seq) == 26
  where seq = sort (filter (\x -> x `elem` ['a', 'b' .. 'z']) (map toLower str))

-----------------------------------------------------------------------
isPangram' :: String -> Bool
isPangram' str = all (`elem` (map toLower str)) ['a'..'z']




{-Description:
Build Tower
Build Tower by the following given argument:
number of floors (integer and always greater than 0).

Tower block is represented as *
for example, a tower of 3 floors looks like below

[
  '  *  ', 
  ' *** ', 
  '*****'
]
and a tower of 6 floors looks like below

[
  '     *     ', 
  '    ***    ', 
  '   *****   ', 
  '  *******  ', 
  ' ********* ', 
  '***********'
]-}


buildTower :: Int -> [String]
buildTower n = map draw [1, 2 .. n]
  where draw i = (take (n-i) [' ', ' ' ..]) ++ 
                 (take (2*i-1) ['*', '*' ..]) ++ 
                 (take (n-i) [' ', ' ' ..])

---------------------------------------------------------------------
buildTower' :: Int -> [String]
buildTower' n = [(sp x++ st x++ sp x) | x <- [1..n]]
    where sp x = replicate (n-x) ' '
          st x = replicate (2*x-1) '*'

----------------------------------------------------------------------
buildFloor :: Int -> Int -> String
buildFloor i n = [if abs j < i then '*' else ' ' | j <- [1 - n .. n - 1]]

buildTower'' :: Int -> [String]
buildTower'' n = [buildFloor i n | i <- [1..n]]

----------------------------------------------------------------------
buildTower''' :: Int -> [String]
buildTower''' 0 = []
buildTower''' x = map (\x -> ' ' : x ++ " ") (buildTower (x - 1)) ++ [replicate (2 * x - 1) '*']






{-My friend John and I are members of the "Fat to Fit Club (FFC)". John is worried because each month a list with the weights of members is published and each month he is the last on the list which means he is the heaviest.

I am the one who establishes the list so I told him: "Don't worry any more, I will modify the order of the list". It was decided to attribute a "weight" to numbers. The weight of a number will be from now on the sum of its digits.

For example 99 will have "weight" 18, 100 will have "weight" 1 so in the list 100 will come before 99. Given a string with the weights of FFC members in normal order can you give this string ordered by "weights" of these numbers?

Example:
"56 65 74 100 99 68 86 180 90" ordered by numbers weights becomes: "100 180 90 56 65 74 68 86 99"

When two numbers have the same "weight", let us class them as if they were strings and not numbers: 100 is before 180 because its "weight" (1) is less than the one of 180 (9) and 180 is before 90 since, having the same "weight" (9) it comes before as a string.

All numbers in the list are positive numbers and the list can be empty.

Notes
it may happen that the input string have leading, trailing whitespaces and more than a unique whitespace between two consecutive numbers
Don't modify the input
For C: The result is freed.-}

orderWeight :: [Char] -> [Char]
orderWeight strng = toStr $ sortBy (compare2) (getList strng)
  where getList str = read ("[" ++ (map (\c -> if c == ' ' then ',' else c) str) ++ "]") :: [Integer]
        toStr list  = map (\c -> if c == ',' then ' ' else c) 
                          (filter (\x -> x /= '[' && x /= ']') $ show list)
        weight num   | num < 10  = num
                     | otherwise = num `mod` 10 + weight (num `div` 10)
        compare2 a b | weight a == weight b = compare (show a) (show b)
                     | otherwise            = comparing weight a b




{-Description:
Write a function that accepts an array of 10 integers (between 0 and 9), that returns a string of those numbers in the form of a phone number.

Example:
createPhoneNumber [1,2,3,4,5,6,7,8,9,0] -- => returns "(123) 456-7890"
The returned format must be correct in order to complete this challenge. 
Don't forget the space after the closing parentheses!-}


createPhoneNumber :: [Int] -> String
createPhoneNumber [a,b,c,d,e,f,g,h,i,j] = printf "(%d%d%d) %d%d%d-%d%d%d%d" a b c d e f g h i j





{--}

module AlternateSplit.JorgeVS.Kata where
import Data.List

encrypt :: String -> Int -> String
encrypt raw n | n <= 0         = raw
              | n `mod` 4 == 0 = raw
              | otherwise      = encrypt (encode raw) (n-1)
  where snds raw i log | i == length raw = log
                       | even (i+1) = snds raw (i+1) ((raw !! i):log)
                       | otherwise  = snds raw (i+1) log
        rest raw i log | i == length raw = log
                       | odd  (i+1) = rest raw (i+1) ((raw !! i):log)
                       | otherwise  = rest raw (i+1) log
        encode raw = reverse (snds raw 0 []) ++ reverse (rest raw 0 [])


decrypt :: String -> Int -> String
decrypt code n | n <= 0    = code
               | otherwise = decrypt (decode code) (n-1)
  where parts = splitAt ((length code) `div` 2) code
        snds  = fst parts; ones  = snd parts
        zip2 [] (y:[]) log = y:log
        zip2 (x:[]) [] log = x:log
        zip2 [] [] log = log
        zip2 (x:xs) (y:ys) log = zip2 xs ys (y:x:log)
        decode code = reverse $ zip2 ones snds []





{-Description:
Complete the function scramble(str1, str2) that returns true if a portion of str1 characters can be rearranged to match str2, otherwise returns false.

Notes:

Only lower case letters will be used (a-z). No punctuation or digits will be included.
Performance needs to be considered
Examples
scramble('rkqodlw', 'world') ==> True
scramble('cedewaraaossoqqyt', 'codewars') ==> True
scramble('katas', 'steak') ==> False-}

scramble :: [Char] -> [Char] -> Bool
scramble s1 s2 = match2 ss1 ss2
  where ss1 = sort $ filter (\x -> x `elem` s2) s1
        ss2 = sort s2
        match2 [] (b:bs) = False
        match2 (a:as) [] = True
        match2 [] [] = True
        match2 (a:as) (b:bs) | a == b = match2 as bs
                             | a /= b = match2 as (b:bs)

----------------------------------------------------------
scramble :: String -> String -> Bool
scramble s1 s2 = s2 \\ s1 == ""




{-Description:
Write a function, which takes a non-negative integer (seconds) as input and 
returns the time in a human-readable format (HH:MM:SS)

HH = hours, padded to 2 digits, range: 00 - 99
MM = minutes, padded to 2 digits, range: 00 - 59
SS = seconds, padded to 2 digits, range: 00 - 59
The maximum time never exceeds 359999 (99:59:59)

You can find some examples in the test fixtures.-}


humanReadable :: Int -> String
humanReadable x = (pad hours) ++ ":" ++ (pad minutes) ++ ":" ++ (pad seconds)
  where pad n | n < 10    = '0':(show n)
              | otherwise = show n
        hours   = (x `div` 3600)
        minutes = (x - hours * 3600) `div` 60
        seconds = (x - hours * 3600 - minutes * 60)

----------------------------------------------------------------------------
humanReadable :: Int -> String
humanReadable x = printf "%02d:%02d:%02d" h m s
  where (y, s) = x `divMod` 60
        (h, m) = y `divMod` 60



{- left about ...

iterate
fold..
<$>, <*>
printf

-}



{-Description:
There is a secret string which is unknown to you. Given a collection of random triplets from the string, 
recover the original string.

A triplet here is defined as a sequence of three letters such that each letter occurs somewhere before 
the next in the given string. "whi" is a triplet for the string "whatisup".

As a simplification, you may assume that no letter occurs more than once in the secret string.

You can assume nothing about the triplets given to you other than that they are valid triplets and 
that they contain sufficient information to deduce the original string. 
In particular, this means that the secret string will never contain letters that 
do not occur in one of the triplets given to you.-}


recoverSecret :: [String] -> String
recoverSecret [] = []
recoverSecret triplets = (getFirst maybes rest) : recoverSecret basket
  where parts  = splitFrom triplets
        maybes = fst parts
        rest   = snd parts
        basket = clean (remove (getFirst maybes rest) triplets []) []


remove c basket hub = case basket of
  [] -> hub
  (s:ss) | head s == c -> remove c ss ((tail s):hub)
         | otherwise -> remove c ss (s:hub)


clean basket hub = case basket of
  [] -> hub
  (s:ss) | s == []   -> clean ss hub
         | otherwise -> clean ss (s:hub)

splitFrom basket = (nub (map head basket), concat (map tail basket))

getFirst maybes rest = case maybes of
  [] -> '?'
  (x:xs) -> if x `elem` rest then getFirst xs rest
            else x
  

------------------------------------------------------------------------

recoverSecret :: [String] -> String
recoverSecret [] = []
recoverSecret triplets = first ++ recoverSecret (filter (not . null) (map (\\ first) triplets))
        where heads = nub $ map head triplets
              first = filter (\x -> all (\y -> (notElem x (tail y))) triplets) heads







{-Description:
Define a function

lastDigit :: Integer -> Integer -> Integer
that takes in two numbers a and b and returns the last decimal digit of a^b. 
Note that a and b may be very large!

For example, the last decimal digit of 9^7 is 9, since 9^7 = 4782969. 
The last decimal digit of (2^200)^(2^300), which has over 10^92 decimal digits, is 6.

The inputs to your function will always be non-negative integers.

Examples
lastDigit 4 1             `shouldBe` 4
lastDigit 4 2             `shouldBe` 6
lastDigit 9 7             `shouldBe` 9
lastDigit 10 (10^10)      `shouldBe` 0
lastDigit (2^200) (2^300) `shouldBe` 6-}


loops = [ [0], [1]
        , [6, 2, 4, 8]
        , [1, 3, 9, 7]
        , [6, 4]
        , [5], [6]
        , [1, 7, 9, 3]
        , [6, 8, 4, 2]
        , [1, 9]]

lastDigit :: Integer -> Integer -> Integer
lastDigit a b = getLoop !! ((fromInteger b) `mod` (length getLoop))
  where getLast n = n `mod` 10
        getLoop = loops !! (fromInteger $ getLast a)

---------------------------------------------------------------------

lastDigit :: Integer -> Integer -> Integer
lastDigit a b = ((a `rem` 10) ^ ((b - 1) `rem` 4 + 1)) `rem` 10






{-给 n 个正整数 a_1,…,a_n， 将 n 个数顺序排成一列后分割成 m 段，每一段的分数被记为这段内所有数的和，
该次分割的分数被记为 m 段分数的最大值。问所有分割方案中分割分数的最小值是多少？

输入描述： 
第一行依次给出正整数 n, m。 
第二行依次给出n 个正整数 a1,...,an。-}


divSeq :: [Integer] -> Integer -> Integer
divSeq list m = try range list m

  where slist = sort list
        total = sum list
        range = [last slist .. total]
        
        divBy w [] acc count = count + 1
        divBy w (x:xs) acc count | acc + x >  w = divBy w (x:xs) 0 (count+1)
                                 | acc + x <= w = divBy w xs (acc+x) count
        try [] list m = -1
        try (x:xs) list m | (divBy x list 0 0) <= m = x
                          | otherwise               = try xs list m








































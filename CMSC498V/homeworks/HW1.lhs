Homework 1
===========

Instructions
------------

The source code of this homework can be found [here](https://raw.githubusercontent.com/nikivazou/CMSC498V/master/CMSC498V/homeworks/HW1.lhs).
You should fill in the definitions of the required functions but **do not** change the types of the functions. 

**How to submit????**

\begin{code}
module HW1 where
import Data.Char 
\end{code}

**Problem 1:** Strings
----------------------

In Haskell the `String` data type is defined to be a list of `Char`acters, 
so `String` can be manipulated via list comprehension. 

For example, bellow list comprehension is used to combine each possible adjectives with each possible noun.

\begin{spec}
[adj ++ " " ++ noun | adj <- ["lazy", "nasty"], noun <- ["cat", "language"] ]
["lazy cat","lazy language","nasty cat","nasty language"]
\end{spec}

You are requested to use list comprehension to define the following three functions on `String`s.

1. Complete the function `removeUpper` that removes all upper case characters from its String argument. 
For example `removeUpper "Hello World!" = "ello orld!"`.
*Hint:* use the library function `isLower`.

\begin{code}
removeUpper :: String -> String
removeUpper xs = error "Define me!"
\end{code}

2. Complete the function `toLowerString` that turns all upper case characters of its String argument to lower. 
For example `toLowerString "Hello World!" = "hello world!"`.
*Hint:* use the library function `toLower`.
\begin{code}
toLowerString :: String -> String
toLowerString xs = error "Define me!"
\end{code}


3. Complete the function `noIdent` that removes any non-letter character of its String argument to lower. 
We defined a letter to be the characters a..z` and `A..Z`.
For example `noIdent "Hello World" = "Hello World"`.
*Hint:* use the library function `elem`.
\begin{code}
noIdent :: String -> String
noIdent xs = error "Define me!"
\end{code}

**Problem 2:** Factoring

We say that `a` is a factor of `n` when there exists an integer `b` so that 
`n = a b`. 

1. Define a function `factors n` that returns all functors of `n`. 
For example, `factors 12 = [1,2,3,4,6,12]` and factors 13 = [1,13].

\begin{code}
factors :: Int -> [Int] 
factors n = error "Define me!"
\end{code}

We say that an integer `n` is prime when its only factors are `1` and `n`. 

2. Define a function `isPrime n` that returns `True` if and only if `n` is prime. 
For example `isPrime 12 = False` and `isPrime 13 = True`.

\begin{code}
isPrime :: Int -> Bool 
isPrime n = error "Define me!"
\end{code}

3. Optimize the `factors n` function to only call the `mod` function 
at most square root n times. 
Note that factors appear in pairs. 
If you found `i` to be a factor of `n`, then 
`n `div` i` is also a factor of `n`. 
*Hint:* define a helper recursive function `factorsRec n i`
and recurse on increasing `i`. 

- Recursion
\begin{code}
factorsOpt :: Int -> [Int]
factorsOpt n = error "Define me!"
\end{code}


4. Test your optimizations. 
The below function `sameElems xs ys` checks that the two input lists have the same elements.

\begin{code}
sameElems :: [Int] -> [Int] -> Bool 
sameElems xs ys = length xs == length ys && all (`elem` ys) xs
\end{code}

Use `sameElems` to write a function `testFactors n` that tests that the two factos functions you wrote above return the same factors. 
*Hint:* use the library function `and`. 

\begin{code}
testFactors :: Int -> Bool 
testFactors n = error "Define me!"
\end{code}

**Problem 3:** Coloring

Let `Color` be the red, green, blue or yellor color data type. 
\begin{code}
data Color 
  = Red | Green | Blue | Yellow 
  deriving (Eq, Show)
\end{code}

*Note* the above `deriving` annotation teaches Haskell 
how to compare colors and turn the to strings for printing. 

Similarly, the `Balcan` data type defines the countries that belong in the 
[Balcan area](https://en.wikipedia.org/wiki/Balkans).
\begin{code}
data Balcan 
  =  Albania | Bulgaria   | BosniaAndHerzegovina 
  |  Kosovo  |  Macedonia | Montenegro
  deriving (Eq, Show)
\end{code}

Two countries are adjacent when they share the same boarder. 
The below `adjacencies` list 
captures all the balcan adjacencies: 
`x` is adjacent to `y` when either `(x,y) `elem` adjacencies` 
or `(y,x) `elem` adjacencies`.

\begin{code}
adjacencies :: [(Balcan,Balcan)]
adjacencies = 
   [ (Albania, Montenegro), (Albania, Kosovo), (Albania, Macedonia)
   , (Bulgaria,Macedonia)
   , (BosniaAndHerzegovina, Montenegro)
   , (Kosovo, Macedonia), (Kosovo, Montenegro)
   ]
\end{code}

We call coloring 
a list of type `[(Balcan,Color)]`
that related each Balcan country with a color. 
A coloring is good with respect to an adjacency matrix 
when every two adjacent countries have a diffent color. 

1. Write a function `isGoodColoring adj coloring`
that returns trus if and only if the `coloring` list is good 
with respect to the input `adj`acency list. 

\begin{code}
isGoodColoring :: [(Balcan, Balcan)] -> [(Balcan,Color)] -> Bool 
isGoodColoring adj coloring = error "Define me!"
\end{code}

2. Write a function `colorings` that returns all the good colorings 
of the adjacency list `adjacencies`. 

\begin{code}
colorings :: [[(Balcan, Color)]]
colorings = error "Define me!"
\end{code}




Haskell 101: Syntax
====================

Haskell is a general purpose language! 
This site is writen in Haskell, check the source code [here](https://github.com/nikivazou/CMSC498V/blob/master/CMSC498V/lectures/Haskell101.lhs)!

\begin{code}
module Main where

import Data.Maybe (isJust, fromJust)
import Data.Char  (toLower)

import Prelude hiding (head, tail, (++), map, foldr, length)
\end{code}

Recursion 
---------

Haskell is a *functional* language: 
supports direct encoding of mathematical functions. 
For example, the [Fibonacci](https://en.wikipedia.org/wiki/Fibonacci_number)
definition

$fib_0 = 0$
$$fib_1 = 1$$
$fib_i = fib_{i-1} + fib_{i-2}$

is directly encoded as 

\begin{code}
fib :: Int -> Int 
fib i = if i == 0 then 0 
        else if i == 1 then 1 
        else fib (i-1) + fib (i-2)
\end{code}

Haskell functions are like math functions: pure (side-effect free).

- *advantage: * everytime you can a function with *same input* it returns *the same* output. 
- *disadvantage:* interaction with the world is tricky. 
- *neutral:* there are no loops! (only recursion)


Running your code
-----------------
To run the `fib` function, you can load this file to `ghci`, a Haskell interpreter. 

< > ghci
< GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
< Prelude> :l Haskell101.lhs
< [1 of 1] Compiling Main             ( Haskell101.lhs, interpreted )
< Ok, modules loaded: Main.


You can ask `ghci` the type or further information about the loaded functions,

< *Main> :t fib
< fib :: Int -> Int
< *Main> :i fib
< fib :: Int -> Int   -- Defined at Haskell101.lhs:30:1


evaluate your code,

< *Main> fib 10
< 55

make new definitions (that went fast!),

< *Main> let fib42 = fib 42

or quit.

< *Main> :q
< Leaving GHCi.

**Q:** Is `fib 42` actually evaluated in the above definition? 

Syntax: Guards 
---------------

Guards provide an alternative syntax for *if* expressions. 
The body of the *first* guard that is evaluated to true 
is returned.

\begin{code}
fib1 :: Int -> Int 
fib1 i 
  | i == 0 || i == 1
  = i 
  | otherwise 
  = fib1 (i-1) + fib1 (i-2)
\end{code}

**Note:** No `=` before the guards!

Syntax: Case Analysis 
-------------

Another alternative syntax is using `case` analysis: 

\begin{code}
fib2 :: Int -> Int 
fib2 i = case i of 
          0 -> 0
          1 -> 1 
          i -> fib2 (i-1) + fib2 (i-2)
\end{code}

**The golden rule of indentation:**
Code which is part of some expression should be indented further in than the beginning of that expression (even if the expression is not the leftmost element of the line).

Violations of this rule lead to syntax error! 


Syntax: Pattern Matching 
------------

A final equivalent syntax is pattern matching: 

\begin{code}
fib3 :: Int -> Int 
fib3 1 = 1 
fib3 0 = 0 
fib3 i = fib3 (i-1) + fib3 (i-2)
\end{code}

**Evaluation order** is from top to bottom. 

**Q:** What happens if the recursive `fib3 i` case is defined first?

**Q:** What is the value of `fib` on negative inputs?

Data Types
---------


Data types classify data for two main purposes. 

**Purpose 1:** Specification to the programmer of the permitted set of operations.  

- `Int`: case analysis, numeric operations (`(+)`, `(-)`, ...)
- `Double`: case analysis, numeric operations (`(/)`, ...)
- `Char`: case analysis, `Data.Char.isUpper`, ...

**Purpose 2:** Specification to the compiler how the programmer intends to use the data.

- `Int` is the type of machine integers, with guaranteed range at least -$2^{29}$ to $2^{29} - 1$.
- `Integer` is arbitrary precision integers, with range as large as you have memory for.

Adding `1` to the largest `Int` will give an overflow. 


< maxBound :: Int 
< 9223372036854775807
< 
< (maxBound + 1) :: Int 
< -9223372036854775808

User Defined Data Types
---------

Users can comlibe data together and provide more operations to them. 
For example, the data `IntError` combines integer values with `Error` string. 

\begin{code}
data IntError 
  = Value {val :: Int} 
  | Error {err :: String} 
\end{code}

Every user defined data type comes with three operations 

- **Construction:** How to construct such the data type 

< Value 42          :: IntError 
< Error "Not Valid" :: IntValue

- **Selection:** Select value from data type 

< *Main> val (Value 42)
< 42
< 
< *Main> err (Value 42)
< *** Exception: No match in record selector err


- **Case Analysis:** How to deconstruct the content of the data

< case val of
<   Value i -> i + 42
<   Error s -> error e

We use `IntError` to return `Error` when `fib` is called on negative numbers. 

\begin{code}
fibError :: Int -> IntError
fibError 0 = Value 0 
fibError 1 = Value 1     -- construction 
fibError i =
  case fibError (i-1) of -- case analysis 
    Value j -> case fibError (i-2) of 
                 Value k -> Value (j+k)
                 Error s -> Error s 
    Error s -> Error s 
\end{code}


Maybe Data Type 
---------

`IntError` is similar to Haskell's `Maybe` data type
that has two constructors `Just` and `Nothing`.

\begin{code}
fibMaybe' :: Int -> Int 
fibMaybe' i = fromJust (fibMaybe i)

fibMaybe :: Int -> Maybe Int 
fibMaybe i | i < 0 = Nothing
fibMaybe 0 = Just 0 
fibMaybe 1 = Just 1 
fibMaybe i = 
  case fibMaybe (i-1) of 
    Just j  -> case fibMaybe (i-2) of 
                Just k  -> Just (j+k)
                Nothing -> Nothing 
    Nothing -> Nothing
\end{code}

**Q:** What is an advantage of using `Maybe` instead of user-defined `IntError`?

Lists 
---------

List is the most famous Haskell data type 
with two constructors 

- the *empty* list `[]` and 
- the *cons* operator `:`. 

Toy List Construction
---------

List construction happens via these two constructors!

< 3:2:1:[]   :: [Int] 
< [3, 2, 1]  :: [Int] -- simplification

Lists can contain any values


< [True]                 :: [Bool]
< ['c', 'h', 'a', 'r']   :: [Char] 
< "char"                 :: String
< [Value 9, Error "pff"] :: [IntError]

**Q:** What is the type of the empty list?

< [] :: ??

**Q:** Is there a type for the cons constructor too? 

< (:) :: ?? 

Case analysis uses the list constructors

< listCase :: [Int] -> Int
< listCase xs = 
<   case xs of 
<     []      -> 1 
<     [2]     -> 2
<     [x,y,z] -> 3
<     x:xs    -> 4
<     [x,y]   -> 5 

**Q:** What is the value of `listCase [2, 6]`?

List Comprehension 
---------

Due to its popularity, 
list manipulation is greatly simplified by list comprehensions. 

- List range

`[lo .. hi]` gives the list of values from `lo` to `hi`.

For example, `[1..10]` gives the list `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`
and `['a'..'z']` gives "abcdefghijklmnopqrstuvwxyz"!


- Filterning  

Ranges can be filtered with predicates that go after the range. 

\begin{code}
evens = [x | x<-[1..10],  x `mod` 2 == 0]
\end{code}

With this we can solve many [Project Euler](https://projecteuler.net/archives) problems!

**Q:** [Problem 1](https://projecteuler.net/problem=1) of Project Euler asks for 
the sum of all the multiples of 3 or 5 below 1000.

\begin{code}
problem1 :: Int 
problem1 = undefined
\end{code}


- Pairs

List comprehensions can compile elements from different ranges

\begin{code}
pairs = [(x,y) | x<-[1..10], y<-[1..10]]
\end{code}


For example, `triangles` give all possible triangles with sizes less than `10`.

\begin{code}
triangles = [(x,y,z) | x<-[1..10], y<-[1..10], z<-[1..10]]
\end{code}


We can get only the right triangles by adding the pythagorean constraint. 
For *efficiency* we search only for sides `x` and `y` that are not greater than the hypotenuse.

\begin{code}
rightTriangles 
  = [(x,y,z) | z<-[1..10], y<-[1..z], x<-[1..z]
             , x^2 + y^2 == z^2]
\end{code}

**Q:** The triangle `(4,3,5)` appears twice as `(4,3,5)` and `(3,4,5)`. How do we filter such duplication?

Finally, we parameterize the right triangle generation on the length of the hypotenuse
to get all right triangles with length up to `n`:

\begin{code}
allRightTriangles n 
  = [(x,y,z) | z<-[1..n], y<-[1..z], x<-[1..y]
             , x^2 + y^2 == z^2]
\end{code}


Recursion on Lists 
---------

List comprehension is great but most of the times we need to use traditional recursion to define functions on lists. 

- Getting the length of a list

\begin{code}
length :: [a] -> Int
length []     = 0 
length (x:xs) = 1 + length xs
\end{code}

< length [1, 2, 3, 4] = 4
< length []           = 0
< length "string"     = 6


**Note:** `length` is _polymorphic_ it operates on lists of every type.     
**Note:** In `Haskell` the `length` function is defined in `Prelude`, that is the module that contain all the basic functions and data definitions and is by default loaded. If you want to redefine functions in `Prelude` you need to explicitly hide the default ones. Look at the beginning of this file to see how indeed we hide the `Prelude.length`. 

- Getting the head of a list

The function `head` returns the head of the list. 
`head` is *partial* that is, it is not defined in all list inputs
as it crashes on empty lists. 

\begin{code}
head :: [a] -> a
head (x:_) = x
head []    = error "head on empty list"
\end{code}

< *Main> head "Haskell"
< 'H'
< *Main> head []
< *** Exception: head on empty list
< CallStack (from HasCallStack):
<  error, called at Haskell101.lhs:391:14 in main:Main

We can just delete the error case from the list definition and Haskell will 
insert a default error call for us!

\begin{code}
head' :: [a] -> a
head' (x:_) = x
\end{code}

< *Main> head' "Haskell"
< 'H'
< *Main> head' []
< *** Exception: Haskell101.lhs:406:1-15: Non-exhaustive patterns in function head'

**Note on polymorphism:** The function `error` is suspicious and we can see it from its type

< error :: String -> a 

It returns any value `a`. In the `head` definition `a` is unified with a list. 

*The rule of polymorphism* is that every function that returns a type variable not appearing in its arguments will crash. 

For example, you can define a non-crashing functions that takes two arguments of type `a` and `b` and returns a `b`. 

\begin{code}
const :: a -> b -> b 
const = undefined 
\end{code}

Now, try to define a non-crashing function that takes two arguments of type `a` and `b` and returns a `c`. 

\begin{code}
challenge :: a -> b -> c 
challenge = undefined 
\end{code}

Back to recursive list functions, let's take the tail of a list!

**Q:** Define the tail of a list

< tail "Yeah Haskell!" = "eah Haskell!"
< tail [1, 2, 3, 4]  = [2, 3, 4]

\begin{code}
tail :: [a] -> [a]
tail = undefined
\end{code}

**Q:** Concatenate two lists

< concat "?eah"   "Haskell!!!!!" = "?eah Haskell!!!!!"
< concat  [-1, 0] [1, 2, 3, 4]   = [-1, 0, 1, 2, 3, 4]

\begin{code}
concat :: [a] -> [a] -> [a]
concat = undefined 
\end{code}

**Note on infix operators**. 
The default list concatenation operator in Haskell is the infix `(++)`.

< "?eah"   ++  "Haskell!!!!!" = "?eah Haskell!!!!!"
< [-1, 0]  ++  [1, 2, 3, 4]   = [-1, 0, 1, 2, 3, 4]

The infix `(++)` is defined as follows 

\begin{code}
(++) :: [a] -> [a] -> [a]
[] ++ ys     = ys
(x:xs) ++ ys = x:(xs ++ ys)
\end{code}

Every infix operator becomes prefix is you wrap it in parenthesis

< (++) "?eah"  "Haskell!!!!!" = "?eah Haskell!!!!!"
< (++) [-1, 0] [1, 2, 3, 4]   = [2, 3, 4]

The other way, every prefix operator becomes infix if you wrap it in "\`".
For example the `elem x xs` function that checks if `x` is an element of the list `xs` can be used as infix

< *Main> elem  1 [1..10]
< True
< *Main> 1 `elem` [1..10]
< True


Computation Patters: mapping
-----------------------------
Let's write a function that converts a string to uppercase. 
Recall that in Haskell, a String is just a list of Char. 
We must start with a function that will convert an individual Char to its uppercase version. Once we find this function, we will simply jog over the list, and apply the function to each Char.

How might we find such a transformer? Lets query Hoogle for a function of the appropriate type! Ah, we see that the module Data.Char contains a function.

< toLower :: Char -> Char

and so now, we can write the simple recursive function

< toLowerString :: String -> String
< toLowerString []     = []
< toLowerString (c:cs) = toLower c : toLowerString cs


Lets now write a function that given a list of integers 
increases each of its elements by 1

< plusOneList :: [Int] -> [Int]
< plusOneList []     = []
< plusOneList (n:ns) = (n+1) : plusOneList ns


Now, in a lesser language, you might be quite happy with the above code. But what separates a good programmer from a great one, is the ability to abstract.

Like humans and monkeys, the functions `toLowerString` and `plusOneList` share 93% of their DNA — the notion of jogging over the list. The common pattern is described by the polymorphic higher-order function map

\begin{code}
map f []     = []
map f (x:xs) = (f x) : (map f xs)
\end{code}

How did we arrive at this? Well, you find what is enshrine in the function’s body that which is common to the different instances, namely the recursive jogging strategy; and the bits that are different, simply become the function’s parameters! Thus, the map function abstracts, or if you have a vivid imagination, locks up in a bottle, the extremely common pattern of jogging over the list.

Verily, the type of map tells us exactly what it does

\begin{code}
map :: (a -> b) -> [a] -> [b]
\end{code}

That is, it takes an `a -> b` transformer and list of a values, and transforms each value to return a list of b values. We can now safely reuse the pattern, by instantiating the transformer with different specific operations.

\begin{code}
toLowerString = map toLower
plusOneList   = map (+1)
\end{code}

Much better.

COMPUTATION PATTERN: FOLDING
-----------------------------

Once you’ve put on the FP goggles, you start seeing computation patterns everywhere.

Lets write a function that adds all the elements of a list.

< listAdd []     = 0
< listAdd (x:xs) = x + (listAdd xs)

Next, a function that multiplies the elements of a list.

< listMul []     = 1
< listMul (x:xs) = x * (listMul xs)

Can you see the pattern? Again, the only bits that are different are the base case value, and the op being performed at each step. We’ll just turn those into parameters, and lo!

\begin{code}
foldr op base []     = base
foldr op base (x:xs) = x `op` (foldr op base xs) 
\end{code}

Now, each of the individual functions are just specific instances of the general foldr pattern.

\begin{code}
listAdd = foldr (+) 0
listMul = foldr (*) 1
\end{code}

To develop some intuition about foldr lets “run” it a few times by hand.

< foldr op base [x1,x2,...,xn] 
< == {- unfold -} 
<    x1 `op` (foldr op base [x2,...,xn])
< == {- unfold -} 
<    x1 `op` (x2 `op` (foldr op base [...,xn]))
< == {- unfold -} 
<    x1 `op` (x2 `op` (... `op` (xn `op` base)))

Aha! It has a rather pleasing structure that mirrors that of lists; the : is replaced by the op and the `[]` is replaced by `base`. Thus, can you see how to use it to eliminate recursion from the recursion from

< listLen []     = 0
< listLen (x:xs) = 1 + (listLen xs)

to define list length in one line. 

\begin{code}
listLen xs = foldr (\_ tailLen -> 1 + tailLen) 0 xs
\end{code}

**Let's Haskell it up!** (a.k.a. make `listLen` shorter but almost unreadable...)

- *Step 1:* Arguments that appear only at the end of the definition can be deleted 

< listLen = foldr (\_ tailLen -> 1 + tailLen) 0 

- *Step 2:* Unused arguments can be simplified with `const` 

< listLen = foldr (const (\tailLen -> 1 + tailLen)) 0 

- *Step 3:* Back to Step 1 :)

< listLen = foldr (const (1+)) 0 


Interaction with the real world
-------------------------------
Now you have learned everything you need to write basic Haskell programs. 
But, if Haskell programs are pure (i.e., same input always gives same output)
wouldn't Haskell programs be obsolete? 
Programs make sense only when they returned (or *output*) different results, 
depending on some user *input*. 

Haskell carefully allows interaction with the real world (input and output) on computations inside the "IO monad".

A function with a "normal" result type is pure. For example none of the following functions can depend on the outside world

< fib               :: Int -> Int 
< allRightTriangles :: Int -> [(Int, Int, Int)]
< id                :: a -> a

Functions that return a value wrapped in `IO` clearly state their dependence on the input and output world. For example `putStrLn` outputs its input string and `getLine` gets an input string from the user. 

< putStrLn :: String -> IO ()
< getLine  :: IO String 

Using the above functions we can ask the user for an integer input, 
compute the `allRightTriangles` of that input and output the result. 

\begin{code}
main :: IO ()
main = do 
  putStrLn "Hello there!" 
  putStrLn "What right triangle are you looking for? "
  i <- getLine 
  let triangles = allRightTriangles (read i)
  putStrLn (show triangles)
\end{code}

All `IO` computations are written inside a `do` notation where there are two kind of defined variable

- pure that are defined using `let` and 
- impure defined using `<-`. 

Doesn't `IO` break Haskell's purity? No! 
All impure computations are carefully wrapped in the `IO` monad! 
Also, Impure `IO` functions can call pure mathematical functions
but not the inverse, 
enforcing a clear separation of the purity boundaries.

Compiling your code
--------------------

After adding interaction with the user, 
this file can be compiled to create an executable program!

Compilation of this file happens with the following command.

< ghc Haskell101.lhs --make

that creates the `Haskell101` executable. The executable runs the `main :: IO ()` function that here computes all the right triangles!
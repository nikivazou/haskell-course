Haskell 101: Syntax
====================

Haskell is a general purpose language! 
This site is writen in Haskell, check the source code [here](https://github.com/nikivazou/CMSC498V/blob/master/CMSC498V/lectures/Haskell101.lhs)!

\begin{code}
module Main where
import Data.Maybe (isJust, fromJust)
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

```
 > ghci
 GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
 Prelude> :l Haskell101.lhs
 [1 of 1] Compiling Main             ( Haskell101.lhs, interpreted )
 Ok, modules loaded: Main.
```


You can ask `ghci` the type or further information about the loaded functions,

```
*Main> :t fib
fib :: Int -> Int
*Main> :i fib
fib :: Int -> Int   -- Defined at Haskell101.lhs:30:1
```

evaluate your code,
```
*Main> fib 10
55
```

make new definitions (that went fast!),

```
let fib42 = fib 42
```

or quit.

```
*Main> :q
Leaving GHCi.
```

**Q:** Is `fib 42` actually evaluated in the above definition? 

Syntax: Guards 
---------------

Guards provide an alternative syntax for *if* expressions. 
The body of the *first* guard that is evaluated to true 
is returned.

\begin{code}
fib1 :: Int -> Int 
fib1 i 
  | i == 0
  = i 
  | i == 1
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

- `Int` is the type of machine integers, with guaranteed range at least -229 to 229 - 1
- `Integer` is arbitrary precision integers, with range as large as you have memory for.

Adding `1` to the largest `Int` will give an overflow. 


```
maxBound :: Int 
9223372036854775807

(maxBound + 1) :: Int 
-9223372036854775808
```

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

```
Value 42          :: IntError 
Error "Not Valid" :: IntValue
```

- **Selection:** Select value from data type 

```
*Main> val (Value 42)
42

*Main> err (Value 42)
*** Exception: No match in record selector err
```

- **Case Analysis:** How to deconstruct the content of the data

```
case val of
  Value i -> i + 42
  Error s -> error e
```

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
fibMaybe :: Int -> Maybe Int 
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

```
3:2:1:[]   :: [Int] 
[3, 2, 1]  :: [Int] -- simplification
```

Case analysis uses the same constructors

```
listCase :: [Int] -> Int
listCase xs = 
  case xs of 
    []      -> 1 
    [2]     -> 2
    [x,y,z] -> 3
    x:xs    -> 4
    [x,y]   -> 5 
```

**Q:** What is the value of `listCase [2, 6]`?

Recursive List Construction
---------

Realistic list construction occurs via recursive functions. 
For example `range lo hi` returns the list of the indexes from `lo` to `hi`.

\begin{code}
range :: Int -> Int -> [Int]
range lo hi 
  | lo <= hi   = lo:range (lo+1) hi
  | otherwise  = [] 
\end{code}


Mapping 
---------

The result list values can be a function of input index

\begin{code}
fibs :: Int -> Int -> [Maybe Int]
fibs lo hi 
  | lo <= hi   = fibMaybe lo:fibs (lo+1) hi
  | otherwise  = [] 
\end{code}

Filtering
---------

Or even a subset of those values depending of a guard. 

\begin{code}
fibs1 :: Int -> Int -> [Int]
fibs1 lo hi 
  | lo <= hi
  , Just f <- fibMaybe lo 
  = f:fibs1 (lo+1) hi
  | lo <= hi                        
  =   fibs1 (lo+1) hi
  | otherwise  
  = [] 
\end{code}


List Comprehension 
---------

Due to its popularity, 
list manipulation is grealty simplified by list comprehensions. 

- List range

List ranging is simplified to 

```
range1 lo hi = [lo..hi]
```

For example, `[1..10]` gives the list `[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]`.


- Filterning  

Ranges can be filtered with predicates that go after the range. 

\begin{code}
evens = [x | x<-[1..10],  x `mod` 2 == 0]
\end{code}

Using list comprehension, `fibs` is implemented as 

\begin{code}
fibs2 lo hi 
  = [fromJust (fibMaybe x) | x <- [lo..hi]
                           , isJust (fibMaybe x)]
\end{code}


- Pairs

List comprehensions can compile elements from different ranges

\begin{code}
pairs = [(x,y) | x<-[1..10], y<-[1..10]]
\end{code}


For example, `triangles` give all possible trianges with sizes less than `10`.

\begin{code}
triangles = [(x,y,z) | x<-[1..10], y<-[1..10], z<-[1..10]]
\end{code}


We can get only the right tringles by adding the pythagorian constraint. 
For *efficiency* we search only for sides `x` and `y` that are not greater than the hypothenuse.

\begin{code}
rightTriangles 
  = [(x,y,z) | z<-[1..10], y<-[1..z], x<-[1..z]
             , x^2 + y^2 == z^2]
\end{code}

**Q:** The triangle `(4,3,5)` appears twice as `(4,3,5)` and `(3,4,5)`. How do we filter such duplication?

Finally, we parameterize the right triangle generation on the length of the hypothenuse
to get all right triangles with length up to `n`:

\begin{code}
allRightTriangles n 
  = [(x,y,z) | z<-[1..n], y<-[1..z], x<-[1..y]
             , x^2 + y^2 == z^2]
\end{code}


Interaction with the real world
-------------------------------

Interaction with the real world (i.e., input, output) is wrapped inside the "IO monad".
The type `IO` encode the non-purity of this interaction: functions returning `IO` computations are no more mathematical functions. 
Impure `IO` functions can call pure mathematical functions
but not the inverse, 
encorcing a clear separation of the pure boundaries. 

\begin{code}
main :: IO ()
main = do 
  putStrLn "Hello World!" 
  putStrLn "What right triangle are you looking for? "
  i <- getLine 
  putStrLn (show (allRightTriangles (read i)))
\end{code}

Compiling your code
--------------------

Haskell provides both the `ghci` intepreter and the `ghc` compiler. 
Compilation of this file `ghc Haskell101.lhs --make` provides the `Haskell101` executable
that you can run to compute all the right triangles!
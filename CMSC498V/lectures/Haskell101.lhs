Haskell 101
============

Haskell is a general purpose language! 
This slide 

\begin{code}
module Haskell101 where
import Data.Maybe (isJust)
\end{code}

Recursion 
------------
\begin{code}
fib :: Int -> Int 
fib i = if i <= 1 then i else fib (i-1) + fib (i-2)
\end{code}

Guards 
------------

\begin{code}
fib1 :: Int -> Int 
fib1 i 
  | i <= 1
  = i 
  | otherwise 
  = fib1 (i-1) + fib1 (i-2)
\end{code}


Case Analysis 
-------------

\begin{code}
fib2 :: Int -> Int 
fib2 i = 
  case i of 
    0 -> 0
    1 -> 1 
    i -> fib'' (i-1) + fib'' (i-2)
\end{code}

Pattern Matching 
------------

\begin{code}
fib'' :: Int -> Int 
fib'' 0 = 0 
fib'' 1 = 1 
fib'' i = fib'' (i-1) + fib'' (i-2)
\end{code}

** Evaluation order**
**What is the value of `fib` on negative inputs?**

- Data Types!

What is a data type?

"A classification of data that allows a specific set of operations" 

- `Int`: case analysis, numeric operations (`(+)`, `(-)`, ...)
- `Double`: case analysis, numeric operations (`(/)`, ...)
- `Char`: case analysis, `Data.Char.isUpper`, ...


"A classification of data that tell the compiler how the programmer intends to use the data"

- `Int` is the type of machine integers, with guaranteed range at least -229 to 229 - 1
- `Integer` is arbitrary precision integers, with range as large as you have memory for.

\begin{code} Adding `1` to the largest `Int` will give an overflow. 
maxBound :: Int 
9223372036854775807

(maxBound + 1) :: Int 
-9223372036854775808
\end{code}

User Defined Data Types
------------------------

Users can comlibe data together and provide more operations to them. 
For example, the data `IntError` combines integer values with Error string. 

\begin{code}
data IntError 
  = Value {val :: Int} 
  | Error {err :: String} 
\end{code}

Every user defined data type comes with two operations 
- **Construction:** How to construct such the data type 
\begin{spec}
Value 42          :: IntError 
Error "Not Valid" :: IntValue
\end{spec} 
- **Case Analysis:** How to deconstruct the content of the data
\begin{spec}
case val of
  Value i -> i + 42
  Error s -> error e
\end{spec}

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
------------------
`IntError` is similar to Haskell's `Maybe` data type
that has two constructors `Maybe` and `Nothing`.
\begin{code}
fibMaybe :: Int -> Maybe Int 
fibMaybe 0 = Just 0 
fibMaybe 1 = Just 1 
fibMaybe i = 
  case fibMaybe (i-1) of 
    Just j -> case fibMaybe (i-2) of 
                  Just k  -> Just (j+k)
                  Nothing -> Nothing 
    Nothing -> Nothing 	 
\end{code}

Lists 
-----
List is the most famous Haskell data type 
with two constructors 
- the *empty* list `[]` and 
- the *cons* operator `:`. 

Toy List Construction
----------------------

\begin{spec}
3:2:1:[]   :: Int 
[3, 2, 1]  :: Int -- simplification
\end{spec}

Case analysis uses the same constructors
\begin{spec}
listCase xs = 
  case xs of 
    []      -> 1 
    [2]     -> 2
    [x,y,z] -> 2
    x:xs    -> 3
    [x,y]   -> 4 
\end{spec}


Recursive List Construction
----------------------------
\begin{code}
range :: Int -> Int -> [Int]
range lo hi 
  | lo <= hi   = lo:range (lo+1) hi
  | otherwise  = [] 
\end{code}


- Mapping 

\begin{code}
fibs :: Int -> Int -> [Maybe Int]
fibs lo hi 
  | lo <= hi   = fibMaybe lo:fibs (lo+1) hi
  | otherwise  = [] 
\end{code}

- Filtering  

\begin{code}
fibs' :: Int -> Int -> [Int]
fibs' lo hi 
  | lo <= hi
  , Just f <- fibMaybe lo 
  = f:fibs' (lo+1) hi
  | lo <= hi                        
  =   fibs' (lo+1) hi
  | otherwise  
  = [] 
\end{code}


List Comprehension 
------------------

\begin{code}
fibsL lo hi = [fibMaybe x | x <- [lo..hi], isJust (fibMaybe x)]
\end{code}

- Pairs 

\begin{code}
pairs = [(x,y) | x<-[1..10], y<-[1..10]]
\end{code}


- Triangles

\begin{code}
triangles = [(x,y) | x<-[1..10], y<-[1..10]]
\end{code}

- right Triangles

\begin{code}
rightTr = [(x,y) | x<-[1..10], y<-[1..10]]
\end{code}


- right Triangles with perimeter 

\begin{code}
perimeter = [(x,y) | x<-[1..10], y<-[1..10]]
\end{code}
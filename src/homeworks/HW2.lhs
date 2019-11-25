Homework 2
===========

Instructions
------------

The source code of this homework can be found [here](https://raw.githubusercontent.com/nikivazou/haskell-course/master/src/homeworks/HW2.lhs).
You should fill in the definitions of the required functions but **do not** change the types of the functions. 

**How to submit:** 
Send an email to `niki.vazou@imdea.org` with 
subject `Haskell-Course'19:HW2` and attach 
this file. 

\begin{code}
module HW2 where
import Prelude hiding (sum, Either(..))
import Data.Monoid
import Control.Parallel.Strategies
\end{code}


**Problem 1:** Eithers are Functors, Applicatives & Monads 
-------------------------------------

The data type `Either a b` contains 

- either a `Left` value `a`, 
- or a `Right` value `b`. 


\begin{code}
data Either a b = Left a | Right b 
  deriving (Show, Eq)
\end{code}

1. **Functors:** 
Define a functor instance of `Either`, that satisfies the functor laws. 
So that, for example:


< ghci> fmap (+42) (Left 0)
< Left 0
< ghci> fmap (+42) (Right 0)
< Right 42 

2. Give a proof that the two functor laws are satisfied by your definition. 

3. **Applicatives:** 
Define an applicative instance of `Either`, that satisfies the applicative laws. 
So that, for example:
 

< ghci> pure 0 :: Either Int Int
< Right 0
< ghci> pure (+42) <*> (Left 0)
< Left 0 
< ghci> pure (+42) <*> (Right 0)
< Right 42 

4. **Monads:** 
Define a monad instance of `Either`, that satisfies the monad laws. 
So that, for example:

\begin{code}
pairs xs ys = do  
  x <- xs 
  y <- ys
  return (x,y)
\end{code}

< ghci> pairs (Right  0) (Right 1)
< Right (0,1)
< ghci> pairs (Right  0) (Left  1)
< Left 1
< ghci> pairs (Left   0) (Right 1)
< Left 0
< ghci> pairs (Left   0) (Left 1)
< Left 0


**Problem 2:** Monadic Lambda Evaluation
-------------------------------------

Given the data type of expressions

\begin{code}
data Exp a = EVar a | EVal Int | EAdd (Exp a) (Exp a)
\end{code}

you are asked to define its monadic instance. 

1. **Functors:** Define the `Functor` instance of `Exp`.

\begin{code}
instance Functor Exp where
  -- fmap :: (a -> b) -> Exp a -> Exp b
  fmap f (EVar x)   = undefined "Define me!"
  fmap f (EVal n)   = undefined "Define me!"
  fmap f (EAdd x y) = undefined "Define me!"
\end{code}

2. **Functor Laws:** Give a proof that the two functor laws are satisfied by your definition. 


3. **Applicatives:** Define the `Applicative` instance of `Exp`.

\begin{code}
instance Applicative Exp where
  -- pure :: a -> Exp a 
  pure x = undefined "Define me!"

  -- (<*>) :: Exp (a -> b) -> Exp a -> Exp b
  ef <*> e = undefined "Define me!"
\end{code}


4. **Monads:** Define the `Monad` instance of `Exp`.

\begin{code}
instance Monad Exp where
  -- return :: a -> Expr a 
  return x = undefined "Define me!"

  -- (>>=) :: Exp a -> (a -> Exp b) -> Exp b 
  (EVar x)   >>= f = undefined "Define me!"
  (EVal n)   >>= f = undefined "Define me!"
  (EAdd x y) >>= f = undefined "Define me!"
\end{code}


5. **Optional** What does the `(>>=)` operator for this type do?


**Problem 3:** Map Reduce
--------------------------

1. **Chunkables:**
The `Chunkable` type class has the method `chunk i x`
that cuts its input `x` into lists of length as most `i`.

\begin{code}
class Chunkable a where 
  chunk :: Int -> a -> [a]
\end{code}

Define lists as chunkable instances so that 

< ghci> chunk 2 [1]
< [[1]]
< ghci> chunk 2 [1..5]
< [[1,2],[3,4],[5]]
< ghci> chunk 6 [1..5]
< [[1,2,3,4,5]]

Generally, each element if `chunk i x` has length no more than `i`, 
and the the chunks exactly reconstruct the list: 

< forall i, x. mconcat (chunk i x) == x

\begin{code}
instance Chunkable [a] where
  chunk = error "Define me!"
\end{code}


2. **Parallel Mapping:** 
Using the parallel functions from the library `Control.Parallel.Strategies`,
we define a parallel mapping function `pmap f xs`
that applies `f` to each element in `xs` in parallel. 

\begin{code}
pmap :: (a -> b) -> [a] -> [b]
pmap = parMap rseq
\end{code}

*Side-Note 1:* If you actually check on the description of `rseq`,
you will discover that `pmap` is not really really parallel. 
For the shake of simplicity, let's assume it is. 

*Side-Note 2:* Parallelization is only possible because the argument function 
is effect-free, as enforced by the type system. 
If `f` had effects, then the order that the effects would be executed, would be undetermined. 

Use `chunk`, `pmap` and a monoid function to define the `mapReduce i f x` function below that 

- chunks the input `x` in chunks of size at most `i`,
- maps `f` to each chunk, in parallel, and 
- concatenates the result list. 

\begin{code}
mapReduce :: (Chunkable a, Monoid b) 
          => Int -> (a -> b) -> (a -> b) 
mapReduce = error "Define me!"
\end{code}

**Hint:** This should be an one line definition!

Then for example, you can parallelize the `sum` function from the lecture: 

\begin{code}
sum :: [Int] -> Sum Int 
sum = mconcat . map Sum  
\end{code}

So that 

< ghci> sum [1..100]
< Sum {getSum = 5050}
< mapReduce 10 sum [1..100]
< Sum {getSum = 5050}

In general: 

< forall xs, i. sum xs = mapReduce i sum xs

Which generalizes to every function `f`

< forall f, i. f = mapReduce i f


3. **Parallel Reducing:** 
As we parallelized mapping, we can also parallelize the "reduce"
stage of map reduce. 

Use `chunk` and `pmap` from before to define a parallelized 
version of the monoid `mconcat` method, so that `pmconcat i xs`

- if `xs` has length less than i, then calls `mconcat`, otherwise
- chunks the input list `xs`, 
- applied `mconcat` in parallel, and 
- recurses on the concatenated chunks.

\begin{code}
pmconcat :: Monoid a => Int -> [a] -> a 
pmconcat = error "Define me!"
\end{code}

**Hint:** `pmconcat` is recursively defined.


Use `pmconcat` to define a "two-level" parallel `mapReduce`, 
that parallelized both the "map" and "reduce" stages: 

\begin{code}
mapReduce2 :: (Chunkable a, Monoid b) 
          => Int -> (a -> b) -> (a -> b) 
mapReduce2 = error "Define me!"
\end{code}

**Hint:** `mapReduce2` can be defined with an one charactet edit from `mapReduce`.


So that 

< mapReduce2 == mapReduce

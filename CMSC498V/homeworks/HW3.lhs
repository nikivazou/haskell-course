Homework 3
===========

Instructions
------------

The source code of this homework can be found [here](https://raw.githubusercontent.com/nikivazou/CMSC498V/master/CMSC498V/homeworks/HW3.lhs).
You should fill in the definitions of the required functions but **do not** change the types of the functions. 

**How to submit:** Submit this file via the submit server. 

\begin{code}
{-# LANGUAGE MultiParamTypeClasses #-}
module HW3 where
import Prelude hiding (sum)
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
Define a functor instance of `Either`. 
So that, for example:


< ghci> fmap (+42) (Left 0)
< Left 0
< ghci> fmap (+42) (Right 0)
< Right 42 

2. **Applicatives:** 
Define an applicative instance of `Either`. 
So that, for example:
 

< ghci> pure 0 :: Either Int Int
< Right 0
< ghci> pure (+42) <*> (Left 0)
< Left 0 
< ghci> pure (+42) <*> (Right 0)
< Right 42 

2. **Monads:** 
Define a monad instance of `Either`. 
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


**Problem 2:** Configurations are monoids 
-------------------------------------

A program configuration forms a monoid. 
For instance, we define the following data type configuration that 
configures how your Haskell program is compiled:

  - `cOpt` shoule the configurations be enabled? (the default is no)
  - `cExt` what language extentions should be used?
  - `cThd` how many threads is the program using?
  
\begin{code}
data Config 
  = Config { cOpt :: Bool
           , cExt :: [String]
           , cThd :: Int 
           }
   deriving (Eq, Show)
\end{code}

1. **Monoidal Configurations:** 
Define a monoid instance of `Config`. 

\begin{code}
instance Monoid Config where
  mempty  = error "Define me!"
  mappend = error "Define me!"
\end{code}

2. **Generalized Configurations:**  
Consinder a generalization of the above configuration, 
where the fields are type variables: 

\begin{code}
data GConfig a b c 
  = GConfig { gcOpt :: a
            , gcExt :: b
            , gcThd :: c
            }
   deriving (Eq, Show)
\end{code}

Now, the instance monoid methods are defined using the monoid methods 
on the generic configuration's fields. 

\begin{code}
instance (Monoid a, Monoid b, Monoid c) => Monoid (GConfig a b c) where
  mempty = GConfig mempty mempty mempty
  mappend (GConfig o1 e1 t1) (GConfig o2 e2 t2) 
         = GConfig (o1 <> o2) (e1 <> e2) (t1 <> t2)
\end{code}

In the class we saw that 

- lists are monoids, but also 
- booleans are monoids (using `All` or `Any`) and 
- integers are monoids (using `Sum` or `Product`).


Use the above monoids to **properly** define a type alias, 
i.e., replace the `Int` below with an instanciation of `GConfig`, 
so that each `MyConfig` has the same information as `Config`, 
i.e., a "boolean", a list integer, and an "integer" fields. 

\begin{code}
type MyConfig = Int 
\end{code}

Define the following functions that convert between your monoid configutations 

\begin{code}
toMyConfig :: Config -> MyConfig
toMyConfig = error "Define me!"

fromMyConfig :: MyConfig -> Config
fromMyConfig = error "Define me!"
\end{code}

and use them to check that your monoid methods behave the same, 
i.e., the following properties are satisfied

< toMyConfig   mempty == mempty
< fromMyConfig mempty == mempty

< forall x, y. toMyConfig   x <> toMyConfig   y == x <> y
< forall x, y. fromMyConfig x <> fromMyConfig y == x <> y


3. **Functor Configurations:** 
Define an instance `Functor` of the generalized configurations, 
that applies functions to the thread field. 

For example, 

< gcThd (fmap (+1)  (GConfig 1 2 3)) == 4 
< gcThd (fmap (*42) (GConfig 1 2 3)) == 126 

Generally, for each function `f` and `GConfig` `c`: 

< forall f, c. gcThd (fmap f c) == f (gcThd c) 


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

Generally, each element if `chunk i x` has lenght no more than `i`, 
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

*Side-Note 2:* Parallelization is only possible because the argement function 
is effect-free, as enforced by the type system. 
If `f` had effects, then the order that the effects would be executed, would be undetermined. 

Use `pmap` to define the `mapReduce i f x` function below that 

- chunks the input `x` in chunks of size at most `i`,
- maps `f` to each chunk, in parallel, and 
- concatenates the result list. 

\begin{code}
mapReduce :: (Chunkable a, Monoid b) 
          => Int -> (a -> b) -> (a -> b) 
mapReduce = error "Define me!"
\end{code}

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

Use `pmconcat` to define a "two-level" parallel `mapReduce`, 
that parallelized both the "map" and "reduce" stages: 

\begin{code}
mapReduce2 :: (Chunkable a, Monoid b) 
          => Int -> (a -> b) -> (a -> b) 
mapReduce2 = error "Define me!"
\end{code}

So that 

< mapReduce2 == mapReduce

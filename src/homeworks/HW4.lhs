Homework 4
===========

Instructions
------------

The source code of this homework can be found [here](https://raw.githubusercontent.com/nikivazou/CMSC498V/master/CMSC498V/homeworks/HW4.lhs).
You should fill in the definitions of the required functions but **do not** change the types of the functions. 

**How to submit:** Submit this file via the submit server. 

\begin{code}
{-# LANGUAGE DeriveFoldable #-}
module HW4 where
import Test.QuickCheck
import Prelude hiding (length, map, foldr, fold1, zipWith, concat, replicate)
{-@ LIQUID "--no-termination" @-}
{-@ LIQUID "--no-totality"    @-}
{-@ LIQUID "--prune-unsorted" @-}
{-@ type True = {v:Bool | v}  @-}
\end{code}


**Problem 1:** Monadic Lambda Evaluation
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



2. **Applicatives:** Define the `Applicative` instance of `Exp`.

\begin{code}
instance Applicative Exp where
  -- pure :: a -> Exp a 
  pure x = undefined "Define me!"

  -- (<*>) :: Exp (a -> b) -> Exp a -> Exp b
  ef <*> e = undefined "Define me!"
\end{code}


3. **Monads:** Define the `Monad` instance of `Exp`.

\begin{code}
instance Monad Exp where
  -- return :: a -> Expr a 
  return x = undefined "Define me!"

  -- (>>=) :: Exp a -> (a -> Exp b) -> Exp b 
  (EVar x)   >>= f = undefined "Define me!"
  (EVal n)   >>= f = undefined "Define me!"
  (EAdd x y) >>= f = undefined "Define me!"
\end{code}


4. **Optional** What does the `(>>=)` operator for this type do?



**Problem 2:** Quick Check List Properties
-------------------------------------------

We define up our own `List` data type from scratch:

\begin{code}
data List a = Nil
            | C a (List a)
            deriving (Eq, Ord, Show, Foldable)
\end{code}

and it's `length` function

\begin{code}
{-@ measure length @-}
{-@ length      :: x:List a -> Nat @-}
length          :: List a -> Int
length Nil      = 0
length (C _ xs) = 1 + length xs
\end{code}


1. Define an arbitrary instance for the `List` data type

\begin{code}
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = undefined "Define me!"
\end{code}


Once you define it, should should be able to quickcheck 
that adding an element to the list increases the length by 1.

\begin{code}
prop_cons :: a -> List a -> Bool 
prop_cons x xs = length (C x xs) == length xs + 1
\end{code}

That is

< ghci> quickCheck prop_cons
< +++ OK, passed 100 tests.



2. Define list concatenation: 

\begin{code}
concat :: List (List a) -> List a 
concat xss = undefined "Define me!"
\end{code}

So that you can quickeck the following property: 

\begin{code}
prop_concat :: List (List a) -> Bool 
prop_concat xss = length (concat xss) == lengths xss
\end{code}

where `lengths` computes the sum of lengths of a list of lists 

\begin{code}
{-@ measure lengths @-}
lengths :: List (List a) -> Int 
lengths Nil      = 0 
lengths (C x xs) = length x + lengths xs 
\end{code}


3. Define the function `replicate i x` 
that returns a list that contains the value `x`, 
`i` times

\begin{code}
replicate :: Int -> a -> List a 
replicate i a = undefined "Define me!"
\end{code}

When you are done, the following property should quickcheck.

\begin{code}
{-@ prop_replicate :: Nat -> a -> Property @-}
prop_replicate :: Int -> a -> Property
prop_replicate n x = 0 <= n ==> n == length (replicate n x)
\end{code}



**Problem 3:** Verify List Properties
-------------------------------------------

We used Quickcheck to gain high confidence on the 
correctness of our definitions but we already observed one 
limitation: `replicate` behaves as expected but only on non-negative inputs. 
What about negative ones? 

1. **Partial Functions:** `replicate`

We can use a Liquid type for `replicate` that specifies
that `replicate` is only defined on `i` that is a natural number. 

\begin{code}
{-@ replicate :: i:Nat -> a -> {v:List a | true } @-}
\end{code}

But what about its result type?
Edit the result type of replicate in the above type, 
so the following is accepted by Liquid Haskell. 


\begin{code}
{-@ prop_replicate_lh :: Nat -> a -> True @-} 
prop_replicate_lh :: Int -> a -> Bool 
prop_replicate_lh n x = n == length (replicate n x)
\end{code}


2. **Higher Order Functions:**
With Liquid Haskell you can also easily check properties 
of higher order functions. 

Refine properly the result type of `map`
\begin{code}
{-@ map :: (a -> b) -> xs:List a -> {v:List b  | true } @-}
map f Nil      = Nil
map f (C x xs) = f x `C` map f xs
\end{code}

So that the below length preserving property is accepted by Liquid Haskell

\begin{code}
{-@ prop_map :: (a -> b) -> List a -> True @-} 
prop_map :: (a -> b) -> List a -> Bool 
prop_map f xs = length xs == length (map f xs)
\end{code}


3. **Higher Order, Partial Functions:**

Finally, fix the specification for `foldr1` so that the call to `error` is
verified by Liquid Haskell, as dead code:

\begin{code}
{-@ foldr1 :: (a -> a -> a) -> {v:List a | true } -> a @-}
foldr1 op (C x xs) = foldr op x xs
foldr1 op Nil      = error "Cannot call foldr1 with empty list"

foldr :: (a -> b -> b) -> b -> List a -> b
foldr _  b Nil       = b
foldr op b (C x  xs) = x `op` (foldr op b xs)
\end{code}


4. Verification of checked properties.

You can use Liquid Haskell to check all the properties 
of Problem 2. 
For the homework, let's just check `prop_concat`:

Give the proper specification to `concat`: 
\begin{code}
{-@ concat :: xs:List (List a) -> {v:List a | true } @-}
\end{code}

So that the length preserving property is checked by Liquid Haskell

\begin{code}
{-@ prop_concat :: List (List a) -> True @-}
\end{code}






Functors and Applicatives
===========================

This lecture is adjusted from [Ranjit Jhala](http://ucsd-pl.github.io/cse230/lectures/lec-monads.html)
who adjusted it from [Graham Hutton](http://www.cs.nott.ac.uk/~gmh/monads).

\begin{code}
module FunctorsAndApplicatives where

import qualified Data.Char as C 
\end{code}


Abstracting programming patterns
---------------------------------

Monoids (and Foldables) are an example of the idea of abstracting out a common programming pattern 
and their properties as a definition. 
Category theory is full of such examples. 
Thus, the goal of this class is not to learn category theory, but instead
how to use these (well studies) abstructions to create good quality of code. 
Our ultimate goal is to reach the abstruction of monads. 
But the path for this goal goes through two simpler categories (or classes in Haskell terms)
that we will see today, Functors and Applicatives. 

Functors: Generalizing mapping
---------------------------

Before considering functors, let us review this idea, by means of two simple functions:

We have seen before the mapping pattern: 

\begin{code}
inc        :: [Int] -> [Int]
inc []     =  []
inc (n:ns) =  n+1 : inc ns

sqr        :: [Int] -> [Int]
sqr []     =  []
sqr (n:ns) =  n^2 : sqr ns

inc' = map (+ 1)
sqr' = map (^ 2)
\end{code}

tx :: (Transformable blob) 
   => (a -> b) -> blob a  -> blob b 


This pattern applied not only to lists, 
but also to trees: 

\begin{code}
data Tree a = Tip | Bin a (Tree a) (Tree a)
  deriving (Eq, Show)
\end{code}

< incr :: Tree Int -> Tree Int 
< incr Tip         = Tip 
< incr (Bin v l r) = Bin (v + 1) (incr l) (incr r)
<
< sqrr :: Tree Int -> Tree Int 
< sqrr Tip         = Tip 
< sqrr (Bin v l r) = Bin (v ^ 2) (sqrr l) (sqrr r)
<
< tincr = tmap (+ 1)
< tsqr  = tmap (^ 2)


Can you spot the pattern?

< tmap :: (a -> b) -> Tree  a -> Tree  b 
< map  :: (a -> b) -> List  a -> List  b

We can either define *different* maps for both the above type constructors, 
or define a type class that abstracts away the mapping idea and re-use 
the exact *same* mapping transformation.

\begin{code}
increase :: (Transformable blob) => blob Int -> blob Int
increase = tx (+ 1)

square   :: (Transformable blob) => blob Int -> blob Int
square   = tx (^ 2)
\end{code}



**Q:** What is the type of `tx`?



Let's now define the `Transformable` type class

\begin{code}
class Transformable t where
  tx :: (a -> b) -> t a -> t b
\end{code}

And next define instances for all the type constructors we care,

\begin{code}
instance Transformable Tree where
  tx _ Tip         = Tip 
  tx f (Bin v l r) = Bin (f v) (tx f l) (tx f r)

instance Transformable [] where
  tx _ []     = []
  tx f (x:xs) = f x : tx f xs

instance Transformable Maybe where 
  tx f Nothing  = Nothing 
  tx f (Just x) = Just (f x) -- :: b 
  -- f :: (a -> b)
  -- Just x :: Maybe a 
  -- x :: a 

instance Transformable IO where 
  tx f a = do x <- a 
              return (f x)

-- main :: IO () 
-- main = do i <- readChar 
-- readChar :: IO Char 
-- i        :: Char 

  -- tx :: (a -> b) -> IO a -> IO b 
\end{code}


Both functions are defined using the same programming pattern, namely mapping the empty list to itself, and a non-empty list to some function applied to the head of the list and the result of recursively processing the tail of the list in the same manner. Abstracting this pattern gives the library function called map

< map         :: (a -> b) -> [a] -> [b]
< map f []     = []
< map f (x:xs) = f x : map f xs

using which our two examples can now be defined more compactly:

< inc = tx (+1)
< sqr = tx (^2)

**Q:** What is the type of `foo` defined as:

< data Maybe a = Just a | Nothing

\begin{code}
foo f (Just x)  = Just (f x)
foo f (Nothing) = Nothing
\end{code}

Ha! It is also a transformation on `Maybe` values. 
Let's go back and make the proper instance! 

**Q:** Define the tranformable instance for `Maybe`.

Generalizing map
-----------------

The same notion of mapping applies to other types, for example, you can imagine:

< mmap   :: (a -> b) -> Maybe a -> Maybe b

or

< allmap :: (a -> b) -> All a -> All b

or

< oimap  :: (a -> b) -> IO a -> IO b

**Q:** Can you define `iomap`?

For this reason, there is a typeclass called `Functor` 
that corresponds to the type constructors that you can `map` over:

< class Functor m where
<   fmap :: (a -> b) -> m a -> m b

Note: The `m` is the type constructor, e.g. `[]` or `IO` or `Maybe`.

**Q:** What is the kind of `m`?

We can make `[]` or `IO` or `Maybe` be instances of `Functor` by:

< instance Functor [] where
<   fmap f []     = []
<   fmap f (x:xs) = f x : fmap f xs

and

< instance Functor Maybe where
<   fmap f Nothing  = Nothing
<   fmap f (Just x) = Just (f x)

and

< instance Functor IO where
<   fmap f x = do {y <- x; return (f x)}


Now we can use `fmap` to generically map functor instances, 
for maybes

< ghci> fmap (+1) Nothing
< Nothing
< ghci> fmap (*2) (Just 3)
< Just 6

for trees 

< ghci > fmap length Tip
< Tip
< ghci > fmap length (Bin "yeah!" Tip Tip)
< Bin 5 Tip Tip


Of course this is too verbose for Haskell!
As in Monoid we can replace `mappend` with `(<>)`, 
in `Functor` you can replace `fmap` with `(<$>)`!
So, `map`, `fmap`, and `(<$>)` all mean exactly the same thing!

The intuition is simple! 
Given a data structure that *contains* elements of some type `a`, 
`fmap f` will map all the `a`'s using `f`. 

**Q:** Let's define the functor instance for trees!

**Q:** Can you define an instance now for an association tree?

\begin{code}
data Map k v = MTip | MBin k v (Map k v) (Map k v)
  deriving (Eq, Show)
\end{code}

**Q:** Can you fmap on an `IO a` action?

Note how `IO` is also some sort of a container of values `a`, 
that are generated using input and output. 

An interesting example of mapping over `IO` actions
is getting the line from the input `getLine`, 
which returns a `String` or a `[Char]` and then 
mapping each character to lower case. 
Thus, we use the same *overloaded* function, to map twice, 
once over lists and once over `IO`.

< fmap (fmap C.toLower) getLine


Functor laws
--------------

Most classes come with laws. 
Lets try to guess the `Functor` laws

< fmap id x      == x

For example, 

< ghci> fmap id [1..3]
< [1,2,3]
< ghci> fmap id "good morning"
< "good morning"

< fmap (f . g) x == fmap f (fmap g x)

For example, 

< ghci> fmap ((+1) . (+2)) [1..3]
< [4,5,6]
< ghci> fmap (toLower . intToDigit) [1..3]
< "123"

**Q:** The second law is call "map-fusion" and is really really important. 
Can you guess why?

Ok, you can just find them under the `Functor` 
class definition in [hackage](https://hackage.haskell.org/package/base-4.10.0.0/docs/Data-Functor.html).

**Q:** Are the lows satisfied by the instances above?


Applicatives: Generalizing function application
-----------------------------------


*General idea:*
Function application 

< f ::   a -> b,   x ::   a, f x ::   b

Generalization into containers

< f :: c (a -> b), x :: c a, f x :: c b

Why would you do that?



Let's generalize map to many arguments.
With one argument, we call it `lift1`

< lift1           :: (a1 -> b) -> [a1] -> [b]
< lift1 f []      = []
< lift1 f (x:xs)  = f x : lift1 f xs
<
< lift1            :: (a1 -> b) -> Maybe a1  -> Maybe b
< lift1 f Nothing  = Nothing
< lift1 f (Just x) = Just (f x)


You can imagine defining a version for two arguments

< lift2 :: (a1 -> a2 -> b) -> [a1] -> [a2] -> [b]
< lift2 f (x:xs) (y:ys) = f x y : lift2 f xs ys
< lift2 f _      _      = []
< 
< lift2 :: (a1 -> a2 -> b) -> Maybe a1 -> Maybe a2 -> Maybe b
< lift2 f (Just x1) (Just x2) = Just (f x1 x2)
< lift2 f _        _          = Nothing

and three arguments and so on

< lift3 :: (a1 -> a2 -> a3 -> b)
<       -> [a1] 
<       -> [a2] 
<       -> [a3] 
<       -> [b]

or

< lift3 :: (a1 -> a2 -> a3 -> b)
<       -> Maybe a1
<       -> Maybe a2
<       -> Maybe a3
<       -> Maybe b

or

< lift3 :: (a1 -> a2 -> a3 -> b)
<       -> IO a1
<       -> IO a2
<       -> IO a3
<       -> IO b

Since we have the `Functor` class defined for `lift1`, 
we should defined `Functor2` for `lift2`, 
`Functor3` for `lift3`, etc!
But, when do we stop?

Applicative
------------

This is annoying! 
For this reason, there is a typeclass called `Applicative` that corresponds to the type constructors that you can `lift2` or `lift3` over.

< class Functor f => Applicative f where
<   pure  :: a -> f a
<   (<*>) :: f (a -> b) -> f a -> f b


**Q:** Assume you have a function `f :: a -> b`
and a container of `a`s `x :: f a`. How would you apply `f` 
to all elements of `a`?

What if `f` had two arguments?  


We can now define all the lifting operators in using the applicative methods. 

< lift2 :: (a1 -> a2 -> b) -> BLOB a1 -> BLOB a2 -> BLOB b
< lift2 f x1 x2 = pure f <*> x1 <*> x2


This definition is in *applicative style* because it looks a lot 
like function application `f x1 x2`. 
But unlike function application that the arguments have types 
`a1`, `a2`, ..., in the applicative style arguments are wrapped 
inside the container `f`: `f a1`, `f a2`, ....

Happily, we do not have to do the lifting ourselves, 
since the `Control.Applicative`
library defines them for us. 


< liftA  :: Applicative t => (a -> b) -> t a -> t b
< 
< liftA2 :: Applicative t => (a1 -> a2 -> b) -> t a1 -> t a2 -> t b
< 
< liftA3 :: Applicative t
<        => (a1 -> a2 -> a3 -> b)
<        -> t a1
<        -> t a2
<        -> t a3
<        -> t b

**Note:** The `t` is the type constructor, e.g. `[]` or `IO` or `Maybe` or `Tree`.


Applicative Instances
----------------------

The standard `Prelude` defines many applicative instances, 
including the Maybe instance: 

< instance Applicative Maybe where
<  -- pure  :: a -> Maybe a
<  -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b

The definition is easy! Just follow the types, 
but the intuition is interesting! 
For the application `f <*> x` to be successful, 
that is return a `Just` both the arguments should succeed!
Applicative for maybes represent *exceptional* programming, 
since they propagate the `Nothing` expeption!


**Q:** What is the values of the following computations?

< pure (+1) <*> Just 1
< pure (+)  <*> Just 1  <*> Just 2
< pure (+)  <*> Nothing <*> Just 2

As maybes represent expeptions, 
lists represent non-determinism: 
a computation can have many different results! 
And the returning list will represent them *all*!

Lets check the boolean operators. 

< ghci> True  && True 
< True 
< ghci> True  && False 
< False 
< ghci> False && True 
< False 
< ghci> False && False 
< False

I can use applicatives to get all the possible outcomes combined!

**Q:** What is the value of 

\begin{code}
andTable = (pure (&&)) <*> [True,False] <*> [True, False]
\end{code}

As a last applicative example we have `IO`!
Below, using the applicative `IO` methods, we define the recursive function 
`getChars` that lifts list `(:)` to combine character reading. 
\begin{code}
getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n-1)
\end{code}

Let's run `getChars 9` to see what happens!  

*Note:* Applicative also have [laws](https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Applicative.html), 
but, kind of more complicated. 
Let's delegate them to the advanced, advanced functional programming. 

Status Check
------------

Our goal is to get all the benefits of effectful programming 
and still be pure! 
The answer to that goal is monads, or ["The essence of functional programming"](https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/the-essence-of-functional-programming.pdf).
It should be clear by now that the road to 
monads and effectful programming
goes through applicatives, since 
applicatives encode *effectful* programming!
Exceptions are represented by `Maybe`, 
non-determinism by Lists, 
interactions by IO, 
and each effect you wish to encode has a representative 
data type (other than divergence!).
So, we are much much closer to understand in essence [monads](Monads.html)!





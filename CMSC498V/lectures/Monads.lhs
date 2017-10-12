Monads
==============

After Monoids, Applicatives and Functors, we are ready to learn what Monads are. 
But, instead of starting with the monadic class definition, 
let's start by an example that is actually using monads in a hidden way.

\begin{code}
{-# LANGUAGE DeriveFunctor #-}
module Monads where
\end{code}

A Simple Evaluator
------------------

Consider the following simple language of expressions that are built up from integer values using a division operator:

\begin{code}
data Expr = Val Int
          | Div Expr Expr
          deriving (Show)
\end{code}

Such expressions can be evaluated as follows:

\begin{code}
eval1              ::  Expr -> Int
eval1 (Val n)     =  n
eval1 (Div x y)   =  eval1 x `div` eval1 y
\end{code}


However, this function doesn’t take account of the possibility of division by zero, 
and will produce an error in this case.

< ghci> eval (Div (Val 1) (Val 0))
< *** Exception: divide by zero 

In order to deal with this explicitly, we can use the `Maybe` type

< data Maybe a = Nothing | Just a

to define a safe version of division

\begin{code}
safeDiv     :: Int -> Int -> Maybe Int
safeDiv n m =  if m == 0 then Nothing else Just (n `div` m)
\end{code}

and then modify our evaluator as follows:


class Monad m where
  return :: a -> m a 
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad [] where 
  return :: a -> [a]
  return x = [x]  
  
  -- (>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= f = concatMap f xs 



*Monads> pairs [1,2,3] "cat" 
-- [(1, 'c'), (2,'a'), (3,'t')]
-- [(1, 'c'), (1,'a'), (1, 't'), ...
    (2,'a'), 
    (3,'t')]
\begin{code}
type Exception a = Maybe a 

eval1' ::  Expr -> Exception Int
eval1' (Val n)   =  Just n
eval1' (Div x y) = do
  n1 <- eval1' x
  n2 <- eval1' y 
  n1 `safeDiv` n2
\end{code}




**Q:** What happens now to our previous exception?

< ghci> eval1' (Div (Val 1) (Val 0))


**Goal:** Simplify `eval1'`.
Let's try to use the fact that `Maybe` is an applicative to 
simplify the definition of `eval1'`: 

<  eval :: Expr -> Maybe Int
<  eval (Val n)   = pure n
<  eval (Div x y) = pure safeDiv <*> eval x <*> eval y

We are in a good direction, but the above does not type check...
`safediv` has type `Int -> Int -> Maybe Int`, 
whereas in the above context a function of type `Int -> Int -> Int` is required.
Replacing `pure safediv` by another function would not help either, 
because this function would need to have type `Maybe (Int -> Int -> Int)`, 
which does not provide any means to indicate failure 
when the second integer argument is zero.

So, `eval` does not fit the pattern of effectful programming 
that is captured by applicative functors. 
They both treat `Maybe` to indicate *failure*, 
yet, applicatives reason about *pure* functions, 
while `safeDiv` observes failure *depending* on its arguments. 

To simplify the nested tuples in `eval`
let's abstact away the  common pattern that 

- performs a case analysis on a value of a `Maybe` type,
- maps `Nothing` to `Nothing`, and
- maps `Just x` to some result depending upon `x`.

Abstract this pattern directly gives a new sequencing operator that we write as 
`(>>=)`:

< (>>=)   :: Maybe a -> (a -> Maybe b) -> Maybe b
< m >>= f =  case m of
<              Nothing -> Nothing
<              Just x  -> f x

Replacing the use of case analysis by pattern matching gives a more compact definition for this operator:

< (>>=)          :: Maybe a -> (a -> Maybe b) -> Maybe b
< Nothing  >>= _ = Nothing
< (Just x) >>= f = f x

That is, if the first argument is `Nothing` then the second argument is ignored and `Nothing` is returned as the result. 
Otherwise, if the first argument is of the form `Just x`, 
then the second argument is applied to `x` to give a result of type `Maybe b`.

The `(>>=)` operator avoids the problem of nested tuples of results because the result of the first argument is made directly available for processing by the second, 
rather than being paired up with the second result to be processed later on. 
In this manner, `(>>=)` integrates the sequencing of values of type `Maybe` with the processing of their result values. 
The function `(>>=)` is read as **bind**
because is nothing fails, the second argument binds the result of the first.     


Using `(>>=)`, our evaluator can now be rewritten as:

\begin{code}
eval (Val n)   = Just n
eval (Div x y) = eval x >>= (\n ->
                   eval y >>= (\m ->
                     safeDiv n m
                   )
                 )
\end{code}

The case for division can be read as follows: 
evaluate `x` and call its result value `n`, 
then evaluate `y` and call its result value `m`, 
and finally combine the two results by applying `safeDiv`. 
In fact, the scoping rules for lambda expressions mean that the parentheses in the case for division can freely be omitted.

Generalising from this example, a typical expression built using the `(>>=)` operator has the following structure:

< m1 >>= \x1 ->
< m2 >>= \x2 ->
< ...
< mn >>= \xn ->
< f x1 x2 ... xn

< do 
<  x1 <- m1 
<  x2 <- m2 
<  ...
<  xn <- mn 
<  f x1 x2 ... xn



That is, evaluate each of the expression `m1`, `m2`,…,`mn` in turn, 
and combine their result values `x1`, `x2`,…, `x`n by applying the function `f`. 
The definition of `(>>=)` ensures that such an expression only succeeds 
(returns a value built using `Just`) if each `mi` in the sequence succeeds.

In other words, the programmer does not have to worry about dealing with the possible failure (returning `Nothing`) of any of the component expressions, 
as this is handled automatically by the `(>>=)` operator.

Haskell provides a special notation for expressions of the above structure, allowing them to be written in a more appealing form:

< do x1 <- m1
<    x2 <- m2
<    ...
<    xn <- mn
<    f x1 x2 ... xn

This is the same notation that is also used for programming with `IO`. 
As in this setting, each item in the sequence must begin in the same column, 
and `xi <- mi` can be abbreviated by `mi` if its result value xi is not required. 

Hence, for example, our evaluator can be redefined as:

< eval (Val n)   = Just n
< eval (Div x y) = do n <- eval x
<                     m <- eval y
<                     safediv n m

**Q:**
Show that the version of eval defined using `(>>=)` is equivalent to our original version, by expanding the definition of `(>>=)`.


Monad is a typeclass 
------------------

The `do` notation for effects is not specific to the `Maybe` or `IO` type, 
but can be used with any applicative type that forms a monad. 
In Haskell, the concept of a monad is captured by the following 
built-in declaration:

< class Applicative m => Monad m where 
<  (>>=)  :: m a -> (a -> m b) -> m b
< 
<  return :: a -> m a
<  return = pure

That is, a monad is an applicative type `m` that supports 
`return` and `(>>=)` functions of the specified types. 
The default definition `return = pure` means that return is normally just
 another name for the applicative function `pure`, 
 but can be overridden in instances declarations if desired.

The function return is included in the Monad class for historical reasons, and to ensure backwards compatibility with existing code, articles and textbooks 
that assume the class declaration includes both `return` and `(>>=)` functions.

The `Maybe` monad
-----------------

It is now straightforward to make Maybe into a monadic type:

< instance Monad Maybe where
<    -- return      :: a -> Maybe a
<    return x       =  Just x
< 
<    -- (>>=)       :: Maybe a -> (a -> Maybe b) -> Maybe b
<    Nothing  >>= _ =  Nothing
<    (Just x) >>= f =  f x

It is because of this declaration that the do notation 
can be used to sequence `Maybe` values. 
In the next few sections we give some further examples of types that are 
monadic, and the benefits that result from recognising and exploiting this fact.



The List Monad
---------------

As with applicatives, 
maybe represents exceptions and lists non-determinism. 
We saw how the (monadic) do notation propagates exceptions 
on maybe values. 

**Q:** Lets define the instance monad for lists. 

< class Applicative m => Monad m where 
<  (>>=)  :: m a -> (a -> m b) -> m b
< 
<  return :: a -> m a
<  return = pure


(Aside: in this context, `[]` denotes the list type `[a]` without its parameter.)
That is, `return` simply converts a value into one result containing that value, 
while `(>>=)` provides a means of combining computations that may produce 
multiple results: `xs >>= f` applies the function `f` to each of the results in 
the list xs to give a nested list of results, 
which is then concatenated to give a single list of results.



As a simple example of the use of the list monad, 
a function that returns all possible ways of pairing elements 
from two lists can be defined using the do notation as follows:

\begin{code}
pairs :: [a] -> [b] -> [(a,b)]
pairs xs ys =  do x <- xs
                  y <- ys
                  return (x, y)
\end{code}

**Q:** What is the value of 

< pairs [1,2,3] "cat"


**Q:** What is the value of 

< pairs [1,2,3] ["cat"]

**Q:** Write pairs using the bind operator. 

That is, consider each possible value x from the list xs, 
and each value y from the list ys, and return the pair (x,y). 
It is interesting to note the similarity to how this function 
would be defined using the list comprehension notation:

< pairs xs ys = [(x, y) | x <- xs, y <- ys]

or in Python syntax:

< def pairs(xs, ys): return [(x,y) for x in xs for y in ys]

In fact, there is a formal connection between the do notation and the comprehension notation. 
Both are simply different shorthands for repeated use of the 
`(>>=)` operator for lists. 
Indeed, the language Gofer that was one of the precursors to Haskell 
permitted the comprehension notation to be used with any monad. 
For simplicity however, Haskell only allows the comprehension 
notation to be used with lists.

**Q:** Can you write the applicativ `andTable`
using do nation?

< andTable = (pure (&&)) <*> [True,False] <*> [True, False]

\begin{code}
andTable = error "Define me!"
\end{code}


Lets see how this computes!

< andTable
< 
<  = do x <- [True, False]
<       y <- [True, False]
<       return (x && y)
< 
<  -- bind notation
<  = ([True, False] >>= (\x -> 
<    ([True, False] >>= (\y -> 
<      return (x && y)
<    )))
< 
<  -- concatMap on x 
<  =  (\x -> 
<      ([True, False] >>= (\y -> 
<        return (x && y)
<     ))) True
<  ++ (\x -> 
<    ([True, False] >>= (\y -> 
<      return (x && y)
<    ))) False
< 
<  -- apply
<  =  [True, False] >>= (\y -> 
<        return (True && y)
<     )
<  ++ [True, False] >>= (\y -> 
<      return (False && y)
<    )
< 
<  -- concatMap on y
<  =  (\y -> 
<        return (True && y)
<     ) True 
<  ++ (\y -> 
<        return (True && y)
<     ) False 
<  ++ (\y -> 
<      return (False && y)
<    ) True 
<  ++ (\y -> 
<      return (False && y)
<    ) False 
< 
<  -- apply
<  =  return (True  && True)
<  ++ return (True  && False)
<  ++ return (False && True)
<  ++ return (False && False)
< 
<  -- booleans
<  =  return True
<  ++ return False
<  ++ return False
<  ++ return False
< 
<  -- return x = [x]
<  =  [True]
<  ++ [False]
<  ++ [False]
<  ++ [False]
< 
<  -- appending
<  =  [True, False, False, False]



Imperative Functional Programming
----------------------------------

Consider the following problem. I have a (finite) list of values, e.g.

< vals0 :: [Char]
< vals0 = ['d', 'b', 'd', 'd', 'a']

that I want to canonize into a list of integers, 
where each distinct value gets the next highest number. 
So I want to see something like

< ghci> canonize vals0
< [0, 1, 0, 0, 2]

similarly, I want:

< ghci> canonize ["zebra", "mouse", "zebra", "zebra", "owl"]
< [0, 1, 0, 0, 2]

**Q:** How would you write `canonize` in Python?

**Q:** How would you write canonize in Haskell?


Now very clean! 
Next let's see how you can get the stateful 
benefits on imperative programming in Haskell 
using a state monad
and how `IO` is just a special case of the state monad.


The State Monad
----------------

Now let us consider the problem of writing functions 
that manipulate some kind of state, 
represented by a type whose internal 
details are not important for the moment:

< type State = ...

The most basic form of function on this type is a state transformer 
(abbreviated by `ST`), which takes the current state as its argument, 
and produces a modified state as its result,
in which the modified state reflects any side effects performed by the function:

< type ST = State -> State

In general, however, we may wish to return a result value in addition to 
updating the state. 
For example, a function for incrementing a counter 
may wish to return the current value of the counter. 
For this reason, we generalise our type of state transformers 
to also return a result value, with the type of such values being 
a parameter of the `ST` type:

\begin{code}
type ST a = State -> (a, State)
\end{code}

Such functions can be depicted as follows, 
where `s` is the input state, `s'` is the output state, 
and `v` is the result value:

![alt text](http://ucsd-pl.github.io/cse230/static/monad1.png "State 1") 


The state transformer may also wish to take argument values. 
However, there is no need to further generalise the `ST` type to take account 
of this, because this behaviour can already be achieved by exploiting currying.
For example, a state transformer that takes a character and returns an integer
would have type `Char -> ST Int`, which abbreviates the curried function type

< Char -> State -> (Int, State)

depicted by:

![alt text](http://ucsd-pl.github.io/cse230/static/monad2.png "State 2") 


Returning to the subject of monads, 
it is now straightforward to make `ST` into an instance of a monadic type:

< instance Monad ST where
<    -- return :: a -> ST a
<    return x  =  \s -> (x,s)
< 
<    -- (>>=)  :: ST a -> (a -> ST b) -> ST b
<    st >>= f  =  \s -> let (x,s') = st s in f x s'

That is, return converts a value into a state transformer that simply returns that value without modifying the state:


![alt text](http://ucsd-pl.github.io/cse230/static/monad3.png "State 3") 


In turn, `(>>=)` provides a means of sequencing state transformers: 
`st >>= f` applies the state transformer `st` to an initial state `s`, 
then applies the function `f` to the resulting value `x` to give a second 
state transformer `(f x)`, 
which is then applied to the modified state `s`' to give the final result:

![alt text](http://ucsd-pl.github.io/cse230/static/monad4.png "State 4") 


Note that `return` could also be defined by `return x s = (x,s)`. 
However, we prefer the above definition in which the second argument `s` 
is shunted to the body of the definition using a lambda abstraction, 
because it makes explicit that `return` is a function that takes a single 
argument and returns a state transformer, 
as expressed by the type `a -> ST a`: 
A similar comment applies to the above definition for `(>>=)`.

We conclude this section with a technical aside. In Haskell, types defined using the type mechanism cannot be made into instances of classes. Hence, in order to make ST into an instance of the class of monadic types, in reality it needs to be redefined using the data mechanism, which requires introducing a dummy constructor (called S for brevity):



*Technicallity:* 
In Haskell, types defined using the type mechanism cannot be made into instances
of classes. 
Hence, in order to make `ST` into an instance of the class of monadic types, 
in reality it needs to be redefined using the data or newtype mechanism, 
which requires introducing a dummy constructor (called S for brevity):

\begin{code}
data ST0 a = S0 (State -> (a, State))
             deriving (Functor)
\end{code}

It is convenient to define our own application function for this type, which simply removes the dummy constructor:

\begin{code}
apply0 :: ST0 a -> State -> (a, State)
apply0 (S0 f) s0 = f s0
\end{code}

In turn, `ST0` is now defined as a monadic type as follows:

\begin{code}
instance Applicative ST0 where
  -- pure :: a -> ST0 a
  pure x   = S0 (\s -> (x, s))
  -- (<*>) :: ST0 (a -> b) -> ST0 a -> ST0 b
  f <*> x  = S0 (\s -> let (f',s')  = apply0 f s  in 
                       let (x',s'') = apply0 x s' in 
                         (f' x',s'')) 

instance Monad ST0 where
  -- (>>=)  :: ST0 a -> (a -> ST0 b) -> ST0 b
  st >>= f   = S0 ( \s -> let (x, s') = apply0 st s in
                          apply0 (f x) s'
                  )
\end{code}


A simple example
-----------

Intuitively, a value of type `ST a` (or `ST0 a`) is simply an action that
returns an a value. The sequencing combinators allow us to combine simple 
actions to get bigger actions, and the `apply0` allows us to execute an action
from some initial state.

**Q:**
To get warmed up with the state-transformer monad, 
lets write a simple sequencing combinator

< (>>) :: Monad m => m a -> m b -> m b

which, in a nutshell, `a1 >>` a2 takes the actions `a1` and `a2` 
and returns the mega action which is 
`a1-then-a2-returning-the-value-returned-by-a2`.


Next, lets see how to implement a “global counter” in Haskell, 
by using a state transformer, in which the internal state 
is simply the next integer

\begin{code}
type State = Int
\end{code}

In order to generate the next integer, 
we define a special state transformer that 
simply returns the current state as its result, 
and the next integer as the new state:

\begin{code}
fresh :: ST0 Int
fresh =  S0 (\n -> (n, n+1))

freshName :: ST0 String
freshName =  S0 (\n -> ("surprise " ++ show n, n + 1))
\end{code}

Note that fresh is a state transformer (where the state is itself just `Int`),
that is an action that happens to return integer values.


**Q:**
Consider the function `surprise` defined as:

\begin{code}
surprise1 = 
  fresh >>= \_ ->
  fresh >>= \_ ->
  fresh >>= \_ ->
  freshName
\end{code}

Indeed, we are just chaining together four fresh actions 
to get a single action that “bumps up” the counter by 4. 
That is, the following versions of `surprise` are also equivalent:


\begin{code}
surprise1' = do 
  fresh
  fresh
  fresh
  fresh

surprise1'' = 
  fresh >> fresh >> fresh >> fresh
\end{code}

Now, the `(>>=)` sequencer is kind of like `(>>)` only it allows you to 
“remember” intermediate values that may have been returned. Similarly,

< return :: a -> ST0 a

takes a value `x` and yields an action that doesnt actually transform the state,
but just returns the same value `x`. 
So, putting things together, how do you think this behaves?

\begin{code}
surprise2 = 
  fresh >>= \n1 ->
  freshName >>= \n2 ->
  fresh >>
  fresh >>
  return (n1, n2)
\end{code}

**Q:** What does the following evaluate to?

< ghci> apply0 surprise2 0

Of course, the do business is just nice syntax for the above:

\begin{code}
surprise2' = do 
  n1 <- fresh
  n2 <- fresh
  fresh
  fresh
  return [n1, n2]
\end{code}

is just like `surprise2`.

A more interesting example
---------------------------

By way of an example of using the state monad, 
let us define a type of binary trees whose
leaves contains values of some type `a`:

\begin{code}
data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Eq, Show)
\end{code}

Here is a simple example:

\begin{code}
tree :: Tree Char
tree =  Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')
\end{code}

Now consider the problem of defining a function that labels each leaf 
in such a tree with a unique or “fresh” integer, 
for example, returning the following:

\begin{code}
tree' =  Node (Node (Leaf ('a', 0)) (Leaf ('b', 1))) (Leaf ('c', 2))
\end{code}


This can be achieved by taking the next fresh integer as an additional argument
to the function, and returning the next fresh integer as an additional result,
for instance, as shown below:

\begin{code}
mlabel :: Tree a -> ST0 (Tree (a,Int))
mlabel (Leaf v)   = error "Define me!"
mlabel (Node l r) = error "Define me!"
\end{code}

Note that the programmer does not have to worry about the tedious and 
error-prone task of dealing with the plumbing of fresh labels, 
as this is handled automatically by the state monad.

Finally, we can now define a function that labels a tree 
by simply applying the
resulting state transformer with zero as the initial state, 
and then discarding the final state:

\begin{code}
label  :: Tree a -> Tree (a, Int)
label t = fst (apply0 (mlabel t) 0)
\end{code}

For example, `label tree` gives the following result:

< ghci> label tree
< Node (Node (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))

**Q:**
Define a function 
`app :: (State -> State) -> ST0 State`, 
such that fresh can be redefined by `fresh = app (+1)`.

**Q:**
Define a function `run :: ST0 a -> State -> a`, 
such that label can be redefined by `label t = run (mlabel t) 0`.

The Generic State Transformer
------------------------------------

The IO Monad
------------

Derived Primitives
------------------

Monads as Programmable Semicolon
---------------------------------

The Monad Laws
---------------

Further Reading
---------------

The subject of monads is a large one, 
and we have only scratched the surface here. 
If you are interested in finding out more, 
two suggestions for further reading would be to look at 
“monads with a zero a plus” 
(which extend the basic notion with two extra primitives that are supported by some monads), 
and “monad transformers” (which provide a means to combine monads). 
For example, see sections 3 and 7 of the following article, 
which concerns the monadic nature of 
[functional parsers](http://www.cs.nott.ac.uk/~gmh/monparsing.pdf). 
For a more in-depth exploration of the IO monad, see Simon Peyton Jones’ excellent article on the [“awkward squad”](http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/).
Monads
==============

After Monoids, Applicatives and Functors, we are ready to learn what Monads are. 
But, instead of starting with the monadic class definition, 
let's start by an example that is actually using monads in a hidden way.

\begin{code}
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
In order to deal with this explicitly, we can use the `Maybe` type

< data Maybe a = Nothing | Just a
< to define a safe version of division

\begin{code}
safeDiv     :: Int -> Int -> Maybe Int
safeDiv n m =  if m == 0 then Nothing else Just (n `div` m)
\end{code}

and then modify our evaluator as follows:

\begin{code}
eval1' ::  Expr -> Maybe Int
eval1' (Val n)   =  Just n
eval1' (Div x y) =  case eval1' x of
                       Nothing -> Nothing
                       Just n1 -> case eval1' y of
                                    Nothing -> Nothing
                                    Just n2 -> n1 `safeDiv` n2
\end{code}

As in the previous section, we can observe a common pattern, 
namely 
performing a case analysis on a value of a `Maybe` type, 
mapping `Nothing` to itself, 
and `Just x` to some result depending upon `x`. 
(Aside: we could go further and also take account of the fact that the case analysis is performed on the result of an eval, but this would lead to the more advanced notion of a monadic fold.)

How should this pattern be abstracted out? 
One approach would be to observe that a key notion in the evaluation of division 
is the sequencing of two values of a `Maybe` type, 
namely the results of evaluating the two arguments of the division. 
Based upon this observation, we could define a sequencing function

\begin{code}
seqn                    :: Maybe a -> Maybe b -> Maybe (a, b)
seqn Nothing   _        =  Nothing
seqn _         Nothing  =  Nothing
seqn (Just x)  (Just y) =  Just (x, y)
\end{code}

using which our evaluator can now be defined more compactly:

\begin{code}
eval2           :: Expr -> Maybe Int
eval2 (Val n)   = Just n
eval2 (Div x y) = apply f (eval2 x `seqn` eval2 y)
                   where f (n, m) = safeDiv n m
\end{code}

**Q:** What must the type of apply be for the above to typecheck?

The auxiliary function `apply` is an analogue of application for `Maybe`, and is used to process the results of the two evaluations:

\begin{code}
apply            :: (a -> Maybe b) -> Maybe a -> Maybe b
apply f Nothing  = Nothing
apply f (Just x) = f x
\end{code}

In practice, however, using `seqn` can lead to programs that manipulate nested tuples, 
which can be messy. 
For example, the evaluation of an operator `Op` with three arguments may be defined by:

< eval (Op x y z) = map f (eval x `seqn` (eval y `seqn` eval z))
<                     where f (a, (b, c)) = ...

Combining Sequencing and Processing
------------------------------------

The problem of nested tuples can be avoided by returning to our original observation of a common pattern:

- performing a case analysis on a value of a `Maybe` type,
- mapping `Nothing` to `Nothing`, and
- mapping `Just x` to some result depending upon `x`.

Abstract this pattern directly gives a new sequencing operator that we write as 
`(>>=)`, and read as “then”:

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
In the literature, `(>>=)` is often called **bind**, because the second argument binds the result of the first.

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
<   m2 >>= \x2 ->
<   ...
<     mn >>= \xn ->
<       f x1 x2 ... xn

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

Hence, for example, our evaluator can be redefined as:

< eval (Val n)   = Just n
< eval (Div x y) = do n <- eval x
<                     m <- eval y
<                     safediv n m

**Q:**
Show that the version of eval defined using `(>>=)` is equivalent to our original version, by expanding the definition of `(>>=)`.

**Q:** Redefine `seqn x y` and `eval (Op x y z)` using the do notation.
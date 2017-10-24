You are Haskellers
------------------

At this point in the course, you know enough to consider yourself Haskellers.
You know what a 
[monad](https://nikivazou.github.io/CMSC498V/lectures/Monads.html) is, 
and even we saw a cool applicatino where 
[testing is monadic](https://nikivazou.github.io/CMSC498V/lectures/Testing.html)! 
If you want to learn more about using Haskell, I encourage you to check out 
the class [resources](https://nikivazou.github.io/CMSC498V/resources.html).

Let me briefly add some further resources:

 * [State of the Haskell ecosystem](https://github.com/Gabriel439/post-rfc/blob/master/sotu.md) is an updated list of many Haskell libs.

 * [Planet Haskell](http://planet.haskell.org/) is a blog aggregator about
   Haskell. There's some very nice reading there.

 * There is a growing number of companies that are interested in Haskell
   programmers, should you ever be looking for a job, including
   [Facebook](https://www.facebook.com/), 
   [Target](https://corporate.target.com/careers/),
   [Awake Networks](http://www.awakenetworks.com/), 
   [Takt](http://takt.com/), 
   [AlphaSheets](http://www.alphasheets.com/), 
   and the list grows every year!
   [Functional Jobs](https://functionaljobs.com/) is a clearinghouse for job
   postings from companies looking to hire people like you.
 
 * [Summer of Haskell](https://summer.haskell.org/) is offering paid Haskell 
   projects every year! 

 * GHC is an open-source project, hosted
   [here](https://ghc.haskell.org/trac/ghc).
   If you see a bug, post a ticket!

 * There are many Haskell venues each year, including
   [ICFP](http://www.icfpconference.org/) and its cool 
   [contest](http://www.icfpconference.org/contest.html), 
   [Haskell Symposium](https://www.haskell.org/haskell-symposium/),
   [Haskell eXchange](https://skillsmatter.com/conferences/10237-haskell-exchange-2018), 
   [lambdaConf](http://lambdaconf.us/), 
   [ZuriHac](https://zurihac.info/),
   [lambda Days](http://www.lambdadays.org/lambdadays2018), 
   [Compose Conference](http://www.composeconference.org/), and 
   [Haskell DC meetup](https://www.meetup.com/Haskell-DC/)! 


 * [Some](https://xkcd.com/1312/) [funny](https://ro-che.info/ccc/1)
   [pages](http://thecodelesscode.com/case/143)
   [about](http://i.imgur.com/6FhL6QJ.jpg)
   [Haskell](https://www.youtube.com/watch?v=Ci48kqp11F8). Notes: Randall
   Munroe writes xkcd; his sidekick, davean, is active in the Haskell
   community. [Ranjit Jhala](https://ranjitjhala.github.io/) is the star in
   the YouTube video (last link) and I was glad he is my academic supervisor!

As a rule, the Haskell community is welcoming, mostly because we've all seen
how awesome Haskell is and we want to share the love!


I will not teach any furter Haskell coding. 
You have learnt everything you need to know! 
In the rest lectures I will show you how use types to reason about your programs. 
Remember we already saw how to use type to test your programs!
We will take this further! 

In this lecture we will see how you can use Haskell's types 
to enforce sophisticated program properties, and next 
we will see how similar reasoning is done using 
[Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell-blog/).


Dependent Haskell
==================

This lecture is adjusted from [Richard Eisenberg's class](https://cs.brynmawr.edu/~rae/courses/17spring380/index.html).



As we've seen a few times, Haskell supports *extensions*, enabling you to turn
on certain language features if you want them. From here on out, we'll be
using a bunch.

> {-# LANGUAGE GADTs                    #-}
> {-# LANGUAGE TypeInType               #-} 
> {-# LANGUAGE ScopedTypeVariables      #-}
> {-# LANGUAGE StandaloneDeriving       #-}
> {-# LANGUAGE TypeFamilies             #-}
> {-# LANGUAGE TypeOperators            #-}
> {-# OPTIONS_GHC -Wincomplete-patterns #-}  -- warn if we forget a case

> module DependentHaskell where

> import Data.Kind  ( Type )
> import Prelude hiding ( reverse, (++), replicate ) -- we'll define our own


Generalized Algebraic Datatypes (GADTs)
---------------------------------------

We have seen the `Maybe` type many many times!

< data Maybe a = Nothing | Just a 

An alternative way to define `Maybe` is the following

```haskell
data Maybe a where
  Nothing :: Maybe a
  Just    :: a -> Maybe a
```

that declares the types of the data constructors `Nothing` and `Just`. 

When we declare the `Nothing` and `Just` constructors, we give them types that
end in `Maybe a`, the datatype we are declaring. But, what if the parameter to
`Maybe` there isn't `a`?

> data G a where
>   MkGInt  :: Int  -> G Int
>   MkGBool :: Bool -> G Bool

> foo :: G a -> a
> foo (MkGInt n)  = n + 5
> foo (MkGBool b) = not b

> bar :: forall a. G a -> a -> Bool
> bar (MkGInt _)  x = ((x :: a) :: Int) > 0
> bar (MkGBool _) x = x

Note that `x` has type `a` here. After the pattern-match, though, we know what
`a` is and can use that fact to produce a `Bool`.

We can now make a definition usable to represent a Haskell type:

> data TypeRep a where
>   TInt    :: TypeRep Int
>   TBool   :: TypeRep Bool
>   TDouble :: TypeRep Double
>   TMaybe  :: TypeRep a -> TypeRep (Maybe a)
>   TFun    :: TypeRep a -> TypeRep b -> TypeRep (a -> b)

> zero :: TypeRep a -> a   -- produces a "zero" of a certain type
> zero TInt       = 0
> zero TBool      = False
> zero TDouble    = 0.0
> zero (TMaybe _) = Nothing
> zero (TFun _ b) = \ _ -> zero b


Data Kinds
----------

We've been talking about algebraic datatypes for months now, so these are nothing
new. But, with the right `LANGUAGE` extensions (I recommend `TypeInType`, but the
older `DataKinds` also works), you can use an algebraic datatype in a *kind*. So,
if we have

> data Nat where
>   Zero :: Nat
>   Succ :: Nat -> Nat

then we can say

> data T :: Nat -> Type where
>   MkT :: String -> T n

With this defintion, the constructor `MkT` (when applied to a `String`) gives us
a `T n` for any `Nat` `n`. For example, we could have `MkT "hi" :: T Zero` or
`MkT "bye" :: T (Succ (Succ Zero))`. Here, `Zero` and `Succ` are being used in
*types*, not ordinary expressions. (They're to the *right* of the `::`.)
So far, this feature looks utterly useless, but it won't be, soon.

One point of complication arises here, though: Haskell has two separate namespaces:
one for constructors and one for types. This is why we can have types like

> data SameName where
>   SameName :: Bool -> SameName

Here, `SameName` is a type and its constructor. This is idiomatic in Haskell, if
confusing for newcomers to the language. Normally, constructors and types are
written in different places in your code, so the re-use of the name isn't
problematic. However, if we can use constructors in types (as we have with `Zero`
and `Succ`) this *is* problematic. Haskell's solution is to use `'` (that is,
an apostrophe) to disambiguate. So, if a name is used as both a constructor and
a type, use `'` in a type to choose the constructor. So, the kind of the type
`SameName` is `Type`, but the kind of the type `'SameName` is `Bool -> SameName`.
GHC prints out constructors in types with the tick-marks.

There is also some new syntax in the definition of `T`: instead of listing
some type variables after the type name `T`, this definition lists `T`'s
*kind*, which is `Nat -> Type`. (`Type` is the more modern way of spelling the
kind `*`. It is imported from `Data.Kind`. `*` gets awfully confusing when you
also have multiplication in types -- which we will have soon enough. In any case,
`Type` and `*` are treated identically.) That is, if the type `T` is given a
`Nat`, it will be a `Type`. This syntax can be used for other constructions.
For example:

< data Tree a = Leaf a
<             | Node a (Tree a) (Tree a)

can be also writen as 

> data Tree :: Type -> Type where
>   Leaf :: Tree a
>   Node :: a -> Tree a -> Tree a -> Tree a

Note that the kind of `Tree` is `Type -> Type`. This is the same as it always
was, but now the kind is written explicitly.

Length-indexed vectors
----------------------

One of the most common examples of a GADT is a length-indexed vector, which we'll
call `Vec`. It is a common example because we can explore all the interesting
aspects of GADTs with them, but they're simpler than many other examples. They
also have a practical use, but it may be some time before we can get there.

Here is the definition of length-indexed vectors:

> data Vec :: Nat -> Type -> Type where
>   Nil  :: Vec Zero a
>   (:>) :: a -> Vec n a -> Vec (Succ n) a
> infixr 5 :>

Before getting all into the types, let's look at what this means at runtime.
A `Vec` is just a list. Compare its definition with that of the ordinary
list type:

```haskell
data [] :: Type -> Type where
  []  :: [] a
  (:) :: a -> [] a -> [] a
infixr 5 :
```

The only difference between these definitions is `Vec`'s `Nat` index.
(The parameter to a type is sometimes called an index -- especially when
that parameter's kind is not `Type`.) Accordingly, you can use `Vec`s wherever
you can use a list.

That `Nat` index tracks the length of a `Vec`. We can see that the index
of `Nil` is always `Zero`. (We see that because the type of `Nil` is always
`Vec Zero a`. You can never have another number there.) We also see that
the index of the result of a cons (that is, a `:>`) is always one more than
the index of the tail of the list. (Here, I'm looking at the `Succ` in the
result type of `(:>)`.)

Let's see some examples. But before we can *see* them, we'll need a `Show`
instance. It would be nice if we could write `deriving Show` in the `Vec`
definition, but normal `deriving` doesn't work with GADTs. (Try it and see
what happens!) So we use another feature called "standalone-deriving" instead:

> deriving instance Show a => Show (Vec n a)

In a standalone-deriving declaration, you write `deriving` away from any
other definition and you give the entire instance header, including any
necessary context. You must also specify the `StandaloneDeriving` language
extension. (If you forget either the context or the extension, GHC helpfully
reminds you. Try this out!)

Now, we can define an example `Vec`:

> stuff = 5 :> 3 :> 8 :> Nil

First off, GHCi can happily print out `stuff`, showing us
`5 :> (3 :> (8 :> Nil))`. Those parentheses can be omitted, but the `Show`
instance isn't quite smart enough. What is `stuff`s type? (Think before you
look.) GHCi reports that it's `Vec ('Succ ('Succ ('Succ 'Zero))) Integer`.
Note the tick-marks in the printout. This type says that the length of the
`Vec` is 3. This should not be terribly surprising.

How can we use this? Let's walk through several examples.

First, we can define a `head` function that is guaranteed to be safe:

> safeHead :: Vec (Succ n) a -> a
> safeHead (x :> _) = x

Despite having only one equation, this function is total. GHC can see that
the index on the type of the argument is `(Succ n)`; therefore, the argument
cannot be `Nil`, whose index is `Zero`. Trying to add an equation
`safeHead Zero = error "urk"` is actually an error with `Inaccessible code`.
(Try it!) Being able to define `safeHead` is already a nice advantage of
use `Vec` over lists.

Naturally, we can have the counterpart to `safeHead`, `safeTail`. But the
type here will be a bit more involved, requiring us to think about the index
of the resulting `Vec`. If the input type's index is `Succ n`, well, the
output type's index had better be `n`:

> safeTail :: Vec (Succ n) a -> Vec n a
> safeTail (_ :> xs) = xs

Once again, this function is total even though it misses the `Nil` case.
Also of interest is that GHC checks to make sure that the return value really
is one element shorter than the input. See what happens if you try
`safeTail xs = xs`. GHC will notice that the index on the input `Vec` is not
`Succ` applied to the index on the output `Vec`.

Let's now write a recursive function, `snoc`. This function (`cons` spelled
backwards) appends to the *end* of a `Vec`. It takes an input `Vec`, a new
element, and produces an output `Vec`, one longer than the input:

> snoc :: Vec n a -> a -> Vec (Succ n) a
> snoc Nil       x = x :> Nil
> snoc (y :> ys) x = y :> snoc ys x

There's quite a bit of heavy lifting going on in the types here. In the
first equation, GHC learns that the index `n` is really `Zero`. So, the
return value must then have type `Vec (Succ Zero) a`. And, sure enough,
following the types of `Nil` and `:>` tells us that `x :> Nil` really
does have type `Vec (Succ Zero) a` (if `x :: a`).

In the second equation, we see that `y :> ys` has type `Vec n a`.
According to the type of `:>`, this means that `ys` must have type
`Vec m a` for some `m` and that `(y :> ys) :: Vec (Succ m) a`. But if
`(y :> ys) :: Vec n a` and `(y :> ys) :: Vec (Succ m) a`, this must
mean that `n` equals `Succ m`. (GHC writes `n ~ Succ m`. The `~` is
GHC's notation for type equality.) Since the return value must have
type `Vec (Succ n) a`, we now know that it must really have the type
`Vec (Succ (Succ m)) a`. Happily, the right-hand side of the equation
above, `y :> snoc ys x` really has that type. First, we see that
`snoc ys x` has type `Vec (Succ m) a` (recalling that `ys :: Vec m a`).
Then, `:>` just adds one more element.


Try playing with this definition to see that GHC will stop you from
making many mistakes. Of course, the types don't track the actual
contents of the `Vec`, so confusing `x` with `y` won't trigger a type
error.


*Untouchable variables* 
If you omit the type of `snoc` you will get a weird type error: 

      Couldn't match expected type t1 with actual type t
        t is untouchable

"Untouchable"? What does that mean? Section 5.2 of [this
paper](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp-outsidein.pdf)
on GHC's type inference algorithm will tell you. (Section 5, up through 5.2, is
actually quite accessible. I'm not joking when I link to the academic paper!)
However, I can summarize: an *untouchable* variable is a type variable
that GHC is unable to infer from the code written. This error almost always
arises from a pattern match over a GADT that does not have a type signature.
Indeed, our `go` helper function does a GADT pattern match, but `go` does not
have a type signature, leading to this error.

The good news about *untouchable* errors is that they are generally straightforward
to fix: just add a type signature. 

We can now use `snoc` in another recursive function, `reverse`:

> reverse :: Vec n a -> Vec n a
> reverse Nil       = Nil
> reverse (x :> xs) = snoc (reverse xs) x

The type of `reverse` tells us that the output `Vec` has the same length
of the input `Vec`. This type also means that GHC checks to make sure the
implementation of `reverse` respects this property.

The definition is not all that remarkable, but it is worth taking the
time to trace through the types, in order to see why `reverse` type-checks.

Concatenating `Vec`s
--------------------

The Haskell Prelude comes with the `(++)` operator on lists:

```haskell
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
```

Translating the function definition to `Vec`s is easy:

> Nil       ++ ys = ys
> (x :> xs) ++ ys = x :> (xs ++ ys)
> infixr 5 ++

Of course, writing this function without a type signature
leads to an *untouchable* error. So we must write a type. Seems simple enough:
the function takes two `Vec`s and outputs a third:

```haskell
(++) :: Vec n a -> Vec m a -> Vec ?????? a
```

The problem, of course, is that the result length is neither `n` nor `m`, the
two input lengths. Instead, it must be the *sum* of `n` and `m`. We can't simply
write `+`, though, because we are working in a *type*, and the `+` that we know
and love is an expression, not a type. Instead, we must define the `+` operation
to work on type-level numbers, using a *type family*. I'll write this type family
in two ways to demonstrate:

> type family Plus (a :: Nat) (b :: Nat) :: Nat where
>   Plus Zero     b = b
>   Plus (Succ a) b = Succ (Plus a b)

> type family a + b where
>   Zero   + b = b
>   Succ a + b = Succ (a + b)
> infixl 6 +

Type families are essentially functions on types. (I say "essentially" because
I think the current design of type families in Haskell is [a bit
wrong](http://cs.brynmawr.edu/~rae/papers/2017/partiality/partiality.pdf).)
They are defined by equations that control the compile-time evaluation of the
type families. So, when we say `Plus Zero (Succ Zero)` in a type, that is
equivalent to `Succ Zero`, according to the first equation. (You can see this
in GHCi by typing `:kind! Plus Zero (Succ Zero)`. Note the `!`, which causes
GHCi to try to evaluate any type families in a type.)

The first definition above uses an alphanumeric name, `Plus`. Because this is
a type, the name of the type must be written with an initial capital letter.
This definition also gives the kinds of the two arguments and the result
(in this case, all `Nat`, but there is no need for these to be the same).

The second definition uses a symbolic name, which can be any symbol, and omits
the kind signature. GHC can use kind inference to figure it all out for you.
There is no support for a standalone kind signature for type families the way
there is for ordinary functions. Also, because the type-level `+` is fully
unrelated to the ordinary `+`, we must give a fixity directive `infixl 6 +`
to get the right precedence and associativity for type-level addition.

Now that we have these type families in hand, we can write the type signature
for `(++)`:

> (++) :: Vec n a -> Vec m a -> Vec (n + m) a

Let's walk through how the definition of `(++)`, above, matches this type.

In the `Nil` case, we learn that `n` is `Zero`. Thus, the output, `ys`, should be
of type `Vec (Zero + m) a`. But by the definition of `(+)`, we see that
`Zero + m` is the same as `m`. So the output type is `Vec m a`, conveniently the
type of `ys`.

In the `:>` case, we learn that `n` is `Succ p` for some `p`. The output type
is now `Vec (Succ p + m) a`. But by the definition of `(+)`, we see that
`Succ p + m` is `Succ (p + m)`, so that the output type is
`Vec (Succ (p + m)) a`. The output expression is `x :> (xs ++ ys)`, where
`xs :: Vec p a` and `ys :: Vec m a`. By the type of `(++)`, we see that
`xs ++ ys :: Vec (p + m) a`, and thus the type of `x :> (xs ++ ys)` is
`Vec (Succ (p + m)) a`, exactly what we want. Huzzah!

GHC does not know how to add
----------------------------

Consider the standard `replicate` function:

```haskell
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : replicate (n-1) x
```

A call to `replicate n x` makes a list containing `n` copies of `x`. Translating
this definition to work over `Vec`s is unsurprising... but the *type* is problematic.
Consider this first draft:

```haskell
replicate :: Int -> a -> Vec n a
```

The problem here is that the `n` in the output type is utterly unrelated to the
input `Int`. That's clearly wrong. A second problem is that the input number
isn't really an *integer*. It should be a natural number. Of course, updating the
type to

< replicate :: Nat -> a -> Vec n a
< replicate Zero      _ = Nil
< replicate (Succ n) x = x :> replicate n x

This gives us grief, though. The grief, in brief, is:

< ghci>  Could not deduce: (m + 'Zero) ~ m
< ghci>  Could not deduce: (m + 'Succ n1) ~ 'Succ (m + n1)

The problem is that GHC does not know how to add. These facts are plainly
true of addition, but it's not obvious to GHC from the definition of `(+)`.
Indeed, when we considered arithmetic on `Nat`s, we had to *prove* these
facts using induction. And, so, we will have to do write these proofs in
Haskell in order for `reverseVec` to type-check. Writing proofs in Haskell
requires singleton types, as we will see. So, let's first explore singleton
types, and then we'll return, once again, to `reverseVec`.

Singletons
----------

What we need is a way of connecting the
term-level, runtime natural number to a type-level, compile-time natural number.
A *singleton type* does this for us. Here is the definition:

> data SNat :: Nat -> Type where
>   SZero :: SNat Zero
>   SSucc :: SNat n -> SNat (Succ n)

This is called a singleton type (or, more accurately, a family of singleton
types... but I won't be that accurate) because, for every index to `SNat`,
there is exactly one inhabitant (ignoring the possibility of infinite
recursion or `undefined` or other sort of cheating). That is, `SNat Zero` has
exactly one inhabitant: `SZero`. `SNat (Succ (Succ Zero))` has exactly one
inhabitant: `SSucc (SSucc SZero)`.

Theorem: For every `n`, `SNat n` has exactly one inhabitant.

Proof: By induction on `n`.

 * Case `n = Zero`: `SZero` inhabits `SNat Zero`. Any other inhabitant would
   need to start with one or more `SSucc`s, but any use of `SSucc` would lead
   to a type index that starts with `Succ`, and `Succ` $\neq$ `Zero`.

 * Case `n = Succ n'`: The induction hypothesis says that there is exactly
   one inhabitant of `SNat n'`. Let's call this `x`. We can see that `SSucc x`
   is an inhabitant of `SNat n` (that is, `SNat (Succ n')`). Are there others?

     * One possibility is that `SZero` inhabits `SNat (Succ n')`. But this is
       impossible because `SZero`'s index is `Zero`, and `Zero` $\neq$ `Succ`.

     * Another possibility is that a sequence of $m$ `SSucc`s followed by an
       `SZero` (where $m>0$) inhabits `SNat (Succ n')`. But in this case,
       removing one `SSucc` from the sequence would be an inhabitant of `SNat
       n'`. Call this `y`. Either `y` equals `x` (in which case we have not
       found an inhabitant distinct from `SSucc x`) or it does not (in which
       case we have a contradiction with our induction hypothesis). Either way,
       our theorem is proved.

QED.

The close correspondence between the term-level value (created with `SZero`
and `SSucc`) and the type-level index (created with `Zero` and `Succ`) means that
the value and the type are isomorphic. Indeed, we can consider them to be
equal, for the right definition of equality.

In practical terms, this means that an argument of type `SNat n` means that a
function can use `n` at runtime *and* at compile-time. The runtime version is the
inhabitant of `SNat n` and the compile-time version is just `n`. But these are
always the same, so we need not consider them separately.

Perhaps going back to the example will make this all clearer. Here is the type
(and body) of `replicate`:

> replicate :: SNat n -> a -> Vec n a
> replicate SZero      _ = Nil
> replicate (SSucc n') x = x :> replicate n' x

The `n` is used in the type because it is the index to the output type `Vec n a`.
It is used in the term because we must pattern match on choice of `n` to determine
how long to make the output list.

Dependent Types in Haskell
---------------------------

Singleton types are a way of faking *dependent types* in Haskell. There are
several dependently typed languages available ([Coq](https://coq.inria.fr/),
[Agda](http://wiki.portal.chalmers.se/agda/pmwiki.php), [Idris](http://www.idris-lang.org/),
and [F*](https://www.fstar-lang.org/) come to mind). 
Making Haskell a dependently typed languages is (up to now) independently 
persuit by [Richard Eisenberg](http://cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis.pdf) via signleton types
and by me! 
In the rest few lectures we will see 
[Liquid Haskell's](https://ucsd-progsys.github.io/liquidhaskell-blog/) 
way to dependent programming, and I believe, it is much simpler! 

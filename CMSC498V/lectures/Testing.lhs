QuickCheck: Type-directed Property Testing
===========================================

\begin{code}
module Testing where
import Test.QuickCheck
import Data.Char
import Data.List (sort, nubBy)
\end{code}

In this lecture, we will look at 
[QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/), 
a technique that cleverly exploits typeclasses and monads to 
deliver a powerful automatic testing methodology.

Quickcheck was developed by 
[Koen Claessen](http://www.cse.chalmers.se/~koen/) 
and [John Hughes](http://www.cse.chalmers.se/~rjmh/)
at 2000 and ten years later won the most influential paper award. 
This was expected, as since then it has been ported to other languages
and is currently used, 
among other things to find subtle 
concurrency bugs in [telecommunications code](http://www.erlang-factory.com/upload/presentations/55/TestingErlangProgrammesforMulticore.pdf).

The key idea on which QuickCheck is founded, 
is *property-based testing*. 
That is, instead of writing individual test cases 
(e.g., unit tests corresponding to input-output pairs for particular functions) 
one should write properties that are desired of the functions, 
and then automatically generate random tests which can be 
run to verify (or rather, falsify) the property.

By emphasizing the importance of specifications, QuickCheck yields several benefits:

1. The developer is forced to **think** about what the code should do,

2. The tool finds **corner-cases** where the specification is violated, which leads to either the code or the specification getting fixed,

3. The specifications live on as rich, machine-checkable **documentation**
about how the code should behave.

Properties
----------

A QuickCheck property is essentially a function whose 
output is a boolean. The standard “hello-world” QC property is

\begin{code}
prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = 
  reverse (xs ++ ys) == reverse xs ++ reverse ys
\end{code}

That is, a property looks a bit like 
a mathematical theorem that the programmer believes is true. 
A QC convention is to use the prefix 'prop_' for QC properties. 
Note that the type signature for the property 
is not the usual polymorphic signature; 
we have given the concrete type `Int` for the elements of the list.
This is because QC uses the types to generate random inputs, 
and hence is restricted to monomorphic properties 
(that don’t contain type variables.)

To check a property, we simply invoke the function

< quickCheck :: (Testable prop) => prop -> IO ()
<     -- Defined in Test.QuickCheck.Test

lets try it on our example property above

< ghci> quickCheck prop_revapp
< *** Failed! Falsifiable (after 2 tests and 1 shrink):
< [0]
< [1]

Whats that ?! Well, lets run the property function on the two inputs

< ghci> prop_revapp [0] [1]
< False

QC has found a sample input for which the property function fails 
i.e., returns `False`. 
Of course, those of you who are paying attention will realize there was a bug in our property, namely it should be

\begin{code}
prop_revapp_ok :: [Int] -> [Int] -> Bool
prop_revapp_ok xs ys = 
  reverse (xs ++ ys) == reverse ys ++ reverse xs
\end{code}

because `reverse` will flip the order of the two parts 
`xs` and `ys` of `xs ++ ys`. 
Now, when we run

< ghci> quickCheck prop_revapp_ok
< +++ OK, passed 100 tests.

That is, Haskell generated 100 test inputs and for all of those, 
the property held. 
You can up the stakes a bit by changing the number of tests 
you want to run

\begin{code}
quickCheckN   :: (Testable p) => Int -> p -> IO ()
quickCheckN n = quickCheckWith $ stdArgs { maxSuccess = n }
\end{code}

and then do

< ghci> quickCheckN 10000 prop_revapp_ok
< +++ OK, passed 10000 tests.


QuickCheck Fancy Sort
---------------------

Lets look at a slightly more interesting example. 
Remember our fancy sorting algorithm?

\begin{code}
fsort :: (Ord a) => [a] -> [a]
fsort = mergeAll . sequences

sequences (a:b:xs)
  | a > b     = descending b [a]  xs
  | otherwise = ascending  b (a:) xs
sequences xs  = [xs]

descending a as (b:bs)
  | a > b          = descending b (a:as) bs
descending a as bs = (a:as): sequences bs

ascending a as (b:bs)
  | a <= b        = ascending b (\ys -> as (a:ys)) bs
ascending a as bs = as [a]: sequences bs

mergeAll [x] = x
mergeAll xs  = mergeAll (mergePairs xs)

mergePairs (a:b:xs) = merge a b: mergePairs xs
mergePairs xs       = xs

merge as@(a:as') bs@(b:bs')
  | a > b           = b:merge as  bs'
  | a < b           = a:merge as' bs
  | otherwise       = a:merge as' bs'
merge [] bs         = bs
merge as []         = as
\end{code}

Lets run it “by hand” on a few inputs

< ghci> [10,9..1]
< [10,9,8,7,6,5,4,3,2,1]
< ghci> fsort [10,9..1]
< [1,2,3,4,5,6,7,8,9,10]

< ghci> [2,4..20] ++ [1,3..11]
< [2,4,6,8,10,12,14,16,18,20,1,3,5,7,9,11]
< ghci> fsort $ [2,4..20] ++ [1,3..11]
< [1,2,3,4,5,6,7,8,9,10,11,12,14,16,18,20]

Looks good -- lets try to test that the output is in fact sorted. 
We need a function that checks that a list is ordered

\begin{code}
isOrdered ::         (Ord a) => [a] -> Bool
isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
isOrdered _          = True
\end{code}

and then we can use the above to write a property

\begin{code}
prop_fsort_isOrdered :: [Int] -> Bool
prop_fsort_isOrdered xs = isOrdered (fsort xs)
\end{code}

and use Quickcheck to test it

< ghci> quickCheckN 10000 isOrdered
< +++ OK, passed 10000 tests.


Conditional Properties
----------------------
Here are several other properties that we might want. 
First, repeated `fsorting` should not change the list. That is,

\begin{code}
prop_fsort_idemp :: [Int] -> Bool
prop_fsort_idemp xs = fsort (fsort xs) == fsort xs
\end{code}

Second, the head of the result is the minimum element of the input

\begin{code}
prop_fsort_min :: [Int] -> Bool
prop_fsort_min xs = head (fsort xs) == minimum xs
\end{code}

However, when we run this, we run into a glitch

< ghci> quickCheck prop_fsort_min
< *** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
< []

**Q:** Can you modify the property `prop_fsort_min` 
to get rid of the runtime exception?

But of course! 
The earlier properties held for all inputs while this property 
makes no sense if the input list is empty! 
This is why thinking about specifications and properties 
has the benefit of clarifying the preconditions 
under which a given piece of code is supposed to work.

In this case we want a conditional properties 
where we only want the output to satisfy to satisfy 
the spec if the input meets the precondition that it is non-empty.

\begin{code}
prop_fsort_nn_min    :: [Int] -> Property
prop_fsort_nn_min xs =
  not (null xs) ==> head (fsort xs) == minimum xs
\end{code}

This time around, both the property holds!

< ghci> quickCheckN 1000 prop_fsort_nn_min
< +++ OK, passed 1000 tests.


**Q:** Can you write a similar property for the maximum of the list?

Note that now, instead of just being a `Bool` 
the output of the function is a `Property` a special type 
built into the `QC` library. 
Similarly the implies combinator `==>` is on of many 
`QC` combinators that allow the construction of rich properties.

< (==>) :: Testable prop => Bool -> prop -> Property

Testing against a model implementation
----------------------------------------

We could keep writing different properties 
that capture various aspects of the desired functionality of `fsort`.
Another approach for validation is to test that our `fsort` is
behaviourally identical to a trusted reference implementation which
itself may be too inefficient or otherwise unsuitable for deployment.
In this case, lets use the standard library’s `sort` function

\begin{code}
prop_fsort_sort    :: [Int] -> Bool
prop_fsort_sort xs =  fsort xs == sort xs
\end{code}

which we can put to the test

< ghci> quickCheckN 1000 prop_fsort_sort
< *** Failed! Falsifiable (after 9 tests and 6 shrink):
< [1,0,0]

Say, what?!

< ghci> fsort [1,0,0]
< [0,1]

Ugh! So close, and yet … Can you spot the bug in our code?

Aha! Merging is implemented to eliminate the duplicates!

Is this a bug in the code? What is a bug anyway? 
Perhaps the fact that the duplicates are eliminated is a feature! 
At any rate there is an inconsistency between our mental model 
of how the code should behave as articulated in `prop_fsort_sort` 
and the actual behavior of the code itself.

We can rectify matters by stipulating that the `fsort` 
is only called on lists of distinct elements

\begin{code}
isDistinct ::(Eq a) => [a] -> Bool
isDistinct (x:xs) = not (x `elem` xs) && isDistinct xs
isDistinct _      = True
\end{code}

and then, weakening the equivalence to only hold on inputs that are duplicate-free

\begin{code}
prop_fsort_distinct_sort :: [Int] -> Property
prop_fsort_distinct_sort xs =
  (isDistinct xs) ==> (fsort xs == sort xs)
\end{code}

QuickCheck happily checks the modified property

< ghci> quickCheck prop_fsort_distinct_sort
< +++ OK, passed 100 tests.

The perils of Conditional Testing
---------------------------------

Well, we managed to fix the `fsort` property, but beware! 
Adding preconditions leads one down a slippery slope. 
In fact, if we paid closer attention to the above runs, 
we would notice something

< ghci> quickCheckN 10000 prop_fsort_distinct_sort
< ...
< (5012 tests; 248 discarded)
< ...
< +++ OK, passed 10000 tests.

The bit about some tests being discarded is ominous. 
In effect, when the property is constructed with the `==>` combinator,
QC discards the randomly generated tests on which the precondition is
false. 
In the above case QC grinds away on the remainder 
until it can meet its target of `10000` valid tests. 
This is because the probability of a randomly generated list meeting 
the precondition (having distinct elements) is high enough. 
This may not always be the case.

The following code is (a simplified version of) the `insert` function from the standard library

\begin{code}
insert x []                 = [x]
insert x (y:ys) | x < y     = x : y : ys
                | otherwise = y : insert x ys
\end{code}

Given an element `x` and a list `xs`, 
the function walks along `xs` till it finds the first
element greater than `x` and it places `x` to the left of that element.
Thus

< ghci> insert 8 ([1..3] ++ [10..13])
< [1,2,3,8,10,11,12,13]

Indeed, the following is the well known insertion-sort algorithm

\begin{code}
isort :: (Ord a) => [a] -> [a]
isort xs = foldr insert [] xs
\end{code}

We could write our own tests, 
but why do something a machine can do better?!

\begin{code}
prop_isort_sort    :: [Int] -> Bool
prop_isort_sort xs = isort xs == sort xs
\end{code}

< ghci> quickCheckN 10000 prop_isort_sort
< +++ OK, passed 10000 tests.

Now, the reason that the above works is that the insert routine preserves sorted-ness. That is while of course the property

\begin{code}
prop_insert_ordered'      :: Int -> [Int] -> Bool
prop_insert_ordered' x xs = isOrdered (insert x xs)
\end{code}

is bogus

< ghci> quickCheckN 10000 prop_insert_ordered'
< *** Failed! Falsifiable (after 4 tests and 1 shrink):
< 0
< [0,-1]

< ghci> insert 0 [0, -1]
< [0, -1, 0]

the output is ordered if the input was ordered to begin with

\begin{code}
prop_insert_ordered      :: Int -> [Int] -> Property
prop_insert_ordered x xs =
  isOrdered xs ==> isOrdered (insert x xs)
\end{code}

Notice that now, the precondition is more complex – the property requires that the input list be ordered. If we QC the property

< ghci> quickCheckN 10000 prop_insert_ordered
< *** Gave up! Passed only 5409 tests.

Ugh! The ordered lists are so sparsely distributed among random lists,
that QC timed out well before it found `10000` valid inputs!

Aside the above example also illustrates the benefit of writing the
property as `p ==> q` instead of using the boolean operator 
`(||)` to write `not p || q`. 
In the latter case, there is a flat predicate, 
and QC doesn’t know what the precondition is, 
so a property may hold vacuously. For example consider the variant

\begin{code}
prop_insert_ordered_vacuous :: Int -> [Int] -> Bool
prop_insert_ordered_vacuous x xs =
  not (isOrdered xs) || isOrdered (insert x xs)
\end{code}

QC will happily check it for us

< ghci> quickCheckN 1000 prop_insert_ordered_vacuous
< +++ OK, passed 10000 tests.

Unfortunately, in the above, 
the tests passed vacuously only because their inputs were not ordered,
and one should use `(==>)` to avoid the false sense of security 
delivered by vacuity.

QC provides us with some combinators for guarding against vacuity by
 allowing us to investigate the distribution of test cases

< collect  :: Show a => a -> Property -> Property
< classify :: Bool -> String -> Property -> Property

We may use these to write a property that looks like

\begin{code}
prop_insert_ordered_vacuous' :: Int -> [Int] -> Property
prop_insert_ordered_vacuous' x xs =
  collect (length xs) $
  classify (isOrdered xs) "ord" $
  classify (not (isOrdered xs)) "not-ord" $
  not (isOrdered xs) || isOrdered (insert x xs)
\end{code}

When we run this, as before we get a detailed breakdown of the `100` passing tests

< ghci> quickCheck prop_insert_ordered_vacuous'
< +++ OK, passed 100 tests:
<  8% 5, not-ord
<  5% 3, not-ord
<  3% 0, ord
<  5% 2, ord
<  5% 12, not-ord
<  5% 10, not-ord
<  2% 1, ord
<  4% 18, not-ord
<  3% 6, not-ord9% 1, ord
< ...

where a line `P% N, COND` means that `p` percent of the inputs had
length `N` and satisfied the predicate denoted by the string `COND`. 
Thus, as we see from the above, 
a paltry `14%` of the tests were ordered and that was because they were either empty 
(`3% 0, ord`) 
or had one (`2% 1, ord`) 
or two elements `(1% 2, ord)`. 
The odds of randomly stumbling upon a beefy list that is ordered are 
rather small indeed!


Generating Data
===============

Before we start discussing how QC generates data 
(and how we can help it generate data meeting some pre-conditions), 
we must ask ourselves a basic question: 
how does QC behave randomly in the first place?!

< ghci> quickCheck prop_insert_ordered'
< *** Failed! Falsifiable (after 4 tests and 2 shrinks):
< 0
< [0,-1]

< ghci> quickCheck prop_insert_ordered'
< *** Failed! Falsifiable (after 5 tests and 5 shrinks):
< 0
< [1,0]

Eh? This seems most impure
-- same inputs yielding two totally different outputs! 
Well, this should give you a clue as to one of the key techniques
underlying QC – monads!

The Generator Monad
-------------------

A Haskell term that generates a (random value) 
of type a has the type `Gen` a which is defined as

< newtype Gen a = MkGen { unGen :: StdGen -> Int -> a }

In effect, the term is a function that takes as input a 
random number generator `StdGen` and a seed `Int` 
and returns an a value. 
One can easily (and we shall see, profitably!) turn 
`Gen` into a `Monad` by

< instance Monad Gen where
<  -- return :: a -> Gen a 
<     return x =
<       MkGen (\_ _ -> x)
<
<  -- (>>=) :: Gen a -> (a -> Gen b) -> Gen b
<     MkGen a >>= f =
<       MkGen (\r n ->
<         let (r1, r2)  = split r
<            MkGen b = f (a r1 n)
<         in b r2 n
<       )

The function `split` simply forks the random number 
generator into two parts; 
which are used by the left and right parameters of the bind 
operator `(>>=)`. 
(Aside you should be able to readily spot the 
similarity between random number generators and the `ST` monad – 
in both cases the basic action is to grab some value and 
transition the state to the next-value.)

The Arbitrary Typeclass
------------------------

QC uses the above to define a typeclass for 
types for which random values can be generated!

< class Show a where
<   show :: a -> String

< class Arbitrary a where
<   arbitrary :: Gen a

Thus, to have QC work with 
(i.e., generate random tests for) 
values of type `a` we need only make `a` an instance of `Arbitrary` 
by defining an appropriate arbitrary function for it. 
QC defines instances for base types like `Int` , `Float`, lists etc.
and lifts them to compound types 
much like we did for JSON a few lectures back.

< instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
<   arbitrary = do x <- arbitrary
<                  y <- arbitrary
<                  return (x,y)

or more simply

< instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
<   arbitrary = (,) <$> arbitrary <*> arbitrary


Generator Combinators
---------------------

QC comes loaded with a set of combinators 
that allow us to create custom instances for our own types.

The first of these combinators is `choose`

< choose :: (System.Random.Random a) => (a, a) -> Gen a

which takes an interval and returns an random element 
from that interval. 
(The typeclass `System.Random.Random` describes types 
which can be sampled. For example, the following is a randomly chosen set of numbers between 0 and 3.

< ghci> sample $ choose (0, 3)
< 2
< 2
< 2
< 1
< 2
< 1
< 0
< 2
< 2
< 1
< 3

**Q:** What is a plausible type for sample?

< Show a => Gen a ->     [a]  -- 1)
<           Gen a -> Gen [a]  -- 2)
< Show a => Gen a -> IO  [a]  -- 3)
< Show a => Gen a -> IO  ()   -- 4)
<           Gen a -> IO  ()   -- 5)
<           Gen a -> IO   a   -- 6)

A second useful combinator is elements

< elements :: [a] -> Gen a

which returns a generator that produces values drawn from the input list

< ghci> sample $ elements [10, 20..100]
< 60
< 70
< 30
< 50
< 30
< 20
< 20
< 10
< 100
< 80
< 10

**Q:** Lets try to figure out the implementation of elements

< elements :: [a] -> Gen a
< elements = error "Define me!"

A third combinator is `oneof`

< oneof :: [Gen a] -> Gen a

which allows us to randomly choose between multiple generators

< ghci> sample $ oneof [elements [10,20,30], choose (0,3)]
< 10
< 0
< 10
< 1
< 30
< 1
< 20
< 2
< 20
< 3
< 30

**Q:** Lets try to figure out the implementation of oneOf

< oneOf :: [Gen a] -> Gen a
< oneOf = error "Define me!"

Finally, `oneOf` is generalized into the frequency combinator

< frequency :: [(Int, Gen a)] -> Gen a

which allows us to build weighted combinations of individual generators.

Generating Ordered Lists
------------------------

We can use the above combinators to write generators for lists

\begin{code}
genList1 ::  (Arbitrary a) => Gen [a]
genList1 = (:) <$> arbitrary <*> genList1
\end{code}

Let's sample it! 

> sample genList1 

Can you spot a problem in the above?

*Problem:* `genList1` only generates infinite lists! 
Hmm. Lets try again,

\begin{code}
genList2 ::  (Arbitrary a) => Gen [a]
genList2 = oneof [ return []
                 , (:) <$> arbitrary <*> genList2]
\end{code}

This is not bad, 
but we may want to give the generator a higher chance of 
not finishing off with the empty list, so lets use

\begin{code}
genList3 ::  (Arbitrary a) => Gen [a]
genList3 = frequency [ (1, return [])
                     , (7, (:) <$> arbitrary <*> genList2) ]
\end{code}

We can use the above to build a custom generator that always returns 
ordered lists by piping the generate list into the sort function

\begin{code}
genOrdList :: (Ord a, Arbitrary a) => Gen [a]
genOrdList = sort <$> genList3
\end{code}

To check the output of a custom generator we can use the
`forAll` combinator

< forAll :: (Show a, Testable prop) 
<        => Gen a -> (a -> prop) -> Property

For example, we can check that in fact, 
the combinator only produces ordered lists

< ghci> quickCheckN 1000 $ forAll genOrdList isOrdered
< +++ OK, passed 1000 tests.

and now, we can properly test the `insert` property

\begin{code}
prop_insert :: Int -> Property
prop_insert x = forAll genOrdList $ 
  \xs -> -- collect (length xs) $ 
         isOrdered xs && isOrdered (insert x xs)
\end{code}

< ghci> quickCheckN 1000 prop_insert
< +++ OK, passed 1000 tests.

Note how the frequency of the empty lists fails when we modify the 
generator frequency. 

Using Quickeck to Grade your Homeworks
================================

Ben has been using QC to grade your homeworks. 
Here are few interesting examples. 


Functional Equivalence: Primality Testing
------------------------------------------

To test your `isPrime` function (HW1, Problem 2)
we defined a model implementation `isPrimeOK`

\begin{code}
factors :: Int -> [Int] 
factors n = [i | i <- [1..n], n `mod` i == 0]

isPrimeOK :: Int -> Bool 
isPrimeOK n = factors n == [1,n]
\end{code} 

and tested the model implementation `isPrimeOK` against  
your submited function, say `isPrimeSol`

\begin{code}
isPrimeSol:: Int -> Bool 
isPrimeSol n = (n < 0) || length (factors n) == 2
\end{code} 

There are many ways to compares these two functions. 
For example, a bad comparison will always return `True`

\begin{code}
prop_isPrime_bad :: Int -> Bool
prop_isPrime_bad _ = True
\end{code}

As another bad check, we check primality on all integers, 
even the negative ones! 

\begin{code}
prop_isPrime_bad_neg :: Int -> Bool
prop_isPrime_bad_neg n = 
  isPrimeOK n == isPrimeSol n
\end{code}


Let's naively restrict checking to positive numbers:

\begin{code}
prop_isPrime_naive :: Int -> Bool
prop_isPrime_naive n = 
  (n <= 0) || isPrimeOK n == isPrimeSol n
\end{code}

We saw that QC behaves more precisely when 
we use `(==>)` to condition testing

\begin{code}
prop_isPrime :: Int -> Property
prop_isPrime n = 
  (n > 0) ==> isPrimeOK n == isPrimeSol n
\end{code}

Yet again, we need to be very cautious when using 
`(==>)` so that we do not exclude meaningful tests:

\begin{code}
prop_isPrime_bad_asm :: Int -> Property
prop_isPrime_bad_asm n = 
  (isPrimeSol n) ==> isPrimeOK n == isPrimeSol n
\end{code}

Or get very restrictive with out assumptions

\begin{code}
prop_isPrime_bad_timeout :: Int -> Property
prop_isPrime_bad_timeout n = 
  (n == 5) ==> isPrimeOK n == isPrimeSol n
\end{code}

Finally, it is always better to use QC's building 
constructs to consruct our tests

\begin{code}
prop_isPrime_best :: (Positive Int) -> Bool
prop_isPrime_best (Positive n) = 
  isPrimeOK n == isPrimeSol n
\end{code}

User Defined Aribitrary Instances: The balkans
------------------------------------------

Recall the HW1, problem 3, the graph coloring problem.

\begin{code}
data Balkan = Albania | Bulgaria | BosniaAndHerzegovina
            | Kosovo | Macedonia | Montenegro
  deriving (Show, Eq)

data Color = Blue | Red | Green | Yellow
  deriving (Show, Eq)

balkans :: [Balkan]
balkans = [Albania, Bulgaria, BosniaAndHerzegovina, Kosovo, Macedonia, Montenegro]

colors :: [Color]
colors = [Blue, Red, Green, Yellow]
\end{code}

Recall the `isGoodColoring` function, which, given
a list of adjacent Balkans and a coloring of those
Balkans, returns true if the coloring was good and
false otherwise. Here are two solutions, the
student solution "_stud" and the teacher solution
"_teach."

\begin{code}
isGoodColoring_teach :: [(Balkan, Balkan)] -> [(Balkan,Color)] -> Bool 
isGoodColoring_teach adj coloring 
  = null [ (c1,c2) | (c1,c2) <- adj, lookup c1 coloring == lookup c2 coloring && lookup c1 coloring /= Nothing]

isGoodColoring_stud :: [(Balkan, Balkan)] -> [(Balkan,Color)] -> Bool 
isGoodColoring_stud adj coloring = null $ do
  (c1, c2) <- adj
  if ((lookup c1 coloring)==(lookup c2 coloring))&& (lookup c1 coloring /= Nothing)
    then
    return (c1, c2)
    else
    []
\end{code}

In order to grade your `isGoodColoring` function, we
first defined simple Arbitrary instances for
both Balkan and Color using `elements`.

\begin{code}
instance Arbitrary Balkan where
  arbitrary = elements balkans

instance Arbitrary Color where
  arbitrary = elements colors
\end{code}

Here is a naive test using only these arbitrary instances:

\begin{code}
testColoringNaive :: [(Balkan, Balkan)] -> [(Balkan, Color)] -> Bool
testColoringNaive adj col =
  isGoodColoring_teach adj col == isGoodColoring_stud adj col
\end{code}

Why might this test be bad?

First, we would like to guarentee that the tested coloring
is "complete":

\begin{code}
sameElems :: Eq a => [a] -> [a] -> Bool 
sameElems xs ys =  length xs == length ys && all (`elem` ys) xs

complete :: [(Balkan, Color)] -> Bool
complete coloring = balkans `sameElems` col_balks
  where
    col_balks = map fst coloring

testColoringComplete :: [(Balkan, Balkan)] -> [(Balkan, Color)] -> Property
testColoringComplete adj col =
  complete col ==>
  isGoodColoring_teach adj col == isGoodColoring_stud adj col
\end{code}

This is better, but now how do we actually generate only
complete colorings?

\begin{code}
genRandCompleteColoring = do
  c1 <- arbitrary
  c2 <- arbitrary
  c3 <- arbitrary
  c4 <- arbitrary
  c5 <- arbitrary
  c6 <- arbitrary  
  return [(Albania, c1), (Bulgaria, c2), (BosniaAndHerzegovina, c3), (Kosovo, c4),
          (Macedonia, c5), (Montenegro, c6)]
\end{code}

Finally, let's guarentee that there is no repetition
in the adjacency list.

\begin{code}
genRandAdjacencies :: Gen [(Balkan, Balkan)]
genRandAdjacencies = do
  adj <- arbitrary
  return $ nubBy (\(x1,y1) (x2,y2) -> (x1,y1) == (x2,y2) || (x1,y1) == (y2, x2)) adj
\end{code}

So now we finally have:

\begin{code}
testColoring :: Property
testColoring =
  forAll genRandCompleteColoring $ \coloring ->
  forAll genRandAdjacencies $ \adj ->
  complete coloring ==>
  isGoodColoring_teach adj coloring == isGoodColoring_stud adj coloring
\end{code}

User Defined Aribitrary Instances: Binary Trees
------------------------------------------

Recall our binary trees from HW2, problem 1.

\begin{code}
data Tree a = Tip | Bin a (Tree a) (Tree a)
  deriving (Eq, Show)
\end{code}

We had you define `map` over trees, as follows:

\begin{code}
map_stud :: (a -> b) -> Tree a -> Tree b 
map_stud _ Tip = Tip 
map_stud f (Bin x l r) = Bin (f x) (map_stud f l) (map_stud f r)

map_teach :: (a -> b) -> Tree a -> Tree b 
map_teach _ Tip = Tip 
map_teach f (Bin x l r) = Bin (f x) (map_teach f l) (map_teach f r) 
\end{code}

How can we create arbitrary Binary trees to pass to map?

Let's try to model this after genList1/2/3 from earlier.

\begin{code}
genArbTree :: (Arbitrary a) => Gen (Tree a)
genArbTree = frequency [ (1, return Tip)
                       , (7, genArbBin)]
\end{code}

This returns either empty or an arbitrary, non-empty
Binary Tree. What does that look like?

\begin{code}
genArbBin :: (Arbitrary a) => Gen (Tree a)
genArbBin = do
  v <- arbitrary
  t1 <- genArbTree
  t2 <- genArbTree
  return $ Bin v t1 t2
\end{code}

Let's define a test for this:

\begin{code}
intFuns :: [Int -> Int]
intFuns = [(+1), (*42),(^3), ((+4) . (*3))]

testMap1 :: Property
testMap1 =
  forAll genArbTree $
  \t ->
    all (\f -> map_teach f t == map_stud f t) intFuns
\end{code}

This looks pretty good... What problems might arise?

Let's try a slightly modified approach:

\begin{code}
genArbTree2 :: (Arbitrary a) => Gen (Tree a)
genArbTree2 = oneof [ return Tip
                    , Bin <$> arbitrary <*> genArbTree2 <*> genArbTree2 ]

genArbTree3 :: (Arbitrary a) => Gen (Tree a)
genArbTree3 = frequency [ (1, return Tip)
                        , (3, Bin <$> arbitrary <*> genArbTree2 <*> genArbTree2)]

testMap2 :: Property
testMap2 =
  forAll genArbTree3 $
  \t ->
    all (\f -> map_stud f t == map_teach f t) intFuns
\end{code}

A final, alternative approach:

\begin{code}
instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized arbTree

arbTree :: Arbitrary a => Int -> Gen (Tree a)
arbTree 0 = do
  return Tip
arbTree n = do
  (Positive m1) <- arbitrary
  (Positive m2) <- arbitrary
  let n1  = n `div` (m1+1)
  let n2 = n `div` (m2+1)
  t1 <- (arbTree n1)
  t2 <- (arbTree n2)
  a <- arbitrary
  return $ Bin a t1 t2

testMap3 :: Tree Int -> Bool
testMap3 t =
    all (\f -> map_stud f t == map_teach f t) intFuns
\end{code}


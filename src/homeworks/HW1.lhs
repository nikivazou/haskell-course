Homework 1
===========

Instructions
------------

The source code of this homework can be found [here](https://raw.githubusercontent.com/nikivazou/haskell-course/master/src/homeworks/HW1.lhs).
You should fill in the definitions of the required functions but **do not** change the types of the functions. 

**How to submit:** 
Send an email to `niki.vazou@imdea.org` with 
subject `Haskell-Course'19:HW1` and attach 
this file and a `MinMax.lhs` file. 

\begin{code}
module HW1 where
\end{code}


**Problem 1:** Factoring
-------------------------

We say that `a` is a factor of `n` when there exists an integer `b` so that 
`n = a * b`. 

1. Define a function `factors n` that returns all functors of `n`. 
For example, `factors 12 = [1,2,3,4,6,12]` and `factors 13 = [1,13]`.

\begin{code}
factors :: Int -> [Int] 
factors n = error "Define me!"
\end{code}

We say that an integer `n` is prime when its only factors are `1` and `n`. 

2. Define a function `isPrime n` that returns `True` if and only if `n` is prime. 
For example `isPrime 12 = False` and `isPrime 13 = True`.

\begin{code}
isPrime :: Int -> Bool 
isPrime n = error "Define me!"
\end{code}

3. Define the function `primes` that returns the list of all prime ints. 

\begin{code}
primes :: [Int]
primes = error "Define me!"
\end{code}

*Hint:* You can define the above function in 20 characters. 
If you want a challenge, you can use 5 more characters and define `primes`
by only using library functions (from `Prelude` and `Data.List`). 


4. Optimize the `factors n` function to only call the `mod` function 
at most <span style="white-space: nowrap; font-size:larger">
&radic;<span style="text-decoration:overline;">&nbsp;n&nbsp;</span>
</span> times. 
Note that factors appear in pairs. 
If you found `i` to be a factor of `n`, then 
`div n i` is also a factor of `n`. 
*Hint:* define a helper recursive function `factorsRec n i`
and recurse on increasing `i`. 

\begin{code}
factorsOpt :: Int -> [Int]
factorsOpt n = error "Define me!"
\end{code}


5. Test your optimization. 
The below function `sameElems xs ys` checks that the two input lists have the same elements, by checking that 

     - the two input lists have same length and
     - all elements of the list `xs` are elements of the list `ys`. 

\begin{code}
sameElems :: [Int] -> [Int] -> Bool 
sameElems xs ys =  length xs == length ys 
                && all (`elem` ys) xs
\end{code}

Use `sameElems` to write a function `testFactors n` that tests that the two factor functions you wrote above return the same factors for every integer up to `n`. 
*Hint:* use the library function `and`. 

\begin{code}
testFactors :: Int -> Bool 
testFactors n = error "Define me!"
\end{code}

**Problem 2:** Merge Sort
------------------------------------

In this problem, you will implement the standard [merge sort](https://en.wikipedia.org/wiki/Merge_sort) algorithm.

The function `mergeSort` splits the input list at halves, 
recursively sorts both halves and merges the two sorted halves. 

\begin{code}
mergeSort :: [Int] -> [Int]
mergeSort []  = [] 
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort ys) (mergeSort zs)
  where 
  (ys,zs)     = splitHalf xs
\end{code}

1. **List splitting:**
Define the function `splitHalf xs` that split the input list into two sublists. 
For example:

< splitHalf [1..4] == ([1,2],[3,4])
< splitHalf [1..5] == ([1,2],[3,4,5])

*Hint:* You can use the function `splitAt`.

\begin{code}
splitHalf :: [a] -> ([a],[a])
splitHalf xs = error "Define me!"
\end{code}

2. **Merging:** Define the function `merge` that merges two sorted lists. 
For example, 

< merge [1, 5, 9] [2,8] == [1,2,5,8,9]

Your definition should satisfy the below type signature that reads as follows: 
The input lists contain elements of type `a` that satisfies the constraint `Ord`. 
This constraint lets you use any of the standard following comparison operators on elements of the input lists

< (<), (<=), (>), (>=) :: Ord a => a -> a -> Bool   

\begin{code}
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys = error "Define me!"
\end{code}


3. **Raising the Ord constraint:**
Haskell defines the data type `Ordering` as follows:

< data Ordering = LT | EQ | GT

to represent less, equal, and greater comparison respectively. 
Moreover the library function compare is defined so that 

< compare x y == LT <=> x <  y
< compare x y == EQ <=> x == y
< compare x y == GT <=> x >  y

Redefine the `merge` and `mergeSort` functions 
to take an explicit comparison argument. That is, 
complete the definitions below so that 

< mergeBy compare xs == merge xs 

\begin{code}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp xs ys     = error "Define me!"

mergeSortBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeSortBy cmp xs ys = error "Define me!"
\end{code}


**Problem 3:** Fancy Sort
------------------------------------

Merge sort is an efficient sorting algorithm, 
but it does not perform well when the input list is already sorted!
The goal of this problem is to implement a different sorting algorithm, 
`fancySort` that performs excellent when the list is already increasing or even 
decreasing. 

The idea behind `fancySort` is the following. 

- First, grap all the sorted sequences that appear in the input list and
- then, merge all the sorted sublists together. 

\begin{code}
fancySort :: Ord a => [a] -> [a]
fancySort = mergeAll . sequences
\end{code}

1. **Merging:** Define the function `mergeAll` that uses the `merge` from Problem 2 
to merge a list of sorted lists into a single sorted list. For example, 

< mergeAll [[1, 5, 9], [2,8], [7]] == [1,2,5,7,8,9]

\begin{code}
mergeAll :: Ord a => [[a]] -> [a]
mergeAll xs = error "Define me!"
\end{code}

The `fancySort` function creates the sorted sequences as follows. 
If the input list `(a:b:xs)` looks like ascending, 
that is `a < b`, then we call `ascending'` with the "accumulated"
sorted list `[a]` and a "pivot" element `b` with the goal to 
collect the rest of the ascending list. Otherwise we call `descending`
with dual arguments and goals. For example, 

< sequences [1, 2, 3, 2, 1] == [[1,2,3],[1,2],[]]
< sequences [1, 2, 3, 4, 5] == [[1,2,3,4,5],[]]
< sequences [5, 4, 3, 2, 1] == [[1,2,3,4,5],[]]

\begin{code}    
sequences :: Ord a => [a] -> [[a]]
sequences (a:b:xs)
  | a < b     = ascending' b [a] xs
  | otherwise = descending b [a] xs
sequences xs  = [xs]
\end{code}

The function `ascending' a as (b:bs)` takes as input 
the pivot element `a`, the increasing accumulator `as`, 
and the unsorted list `xs`. If the pivot is less than `b`, 
it puts the pivot in the head of the accumulator and recurses, 
otherwise, it returns the first sorted sequence `reverse(a:as)`
and calls back to `sequences` to construct the next descending sequence.

\begin{code}
ascending' :: Ord a => a -> [a] -> [a] -> [[a]]
ascending' a as (b:bs)
  | a < b = ascending' b (a:as) bs
ascending' a as bs = (reverse(a:as)):sequences bs
\end{code}

2. **Define descending:** Follow the definition of `ascending'` to define `descending`
that returns the first descending sequence it finds and calls back to 
`sequences` for the rest. For example, 

< descending 4 [] [3, 2, 1]       == [[1,2,3,4],[]]
< descending 0 [] [3, 2, 1]       == [[0],[1,2,3],[]]
< descending 0 [] [1, 2, 3, 2, 1] == [[0],[1,2,3],[1,2],[]]

Note that `descending` should only find strictly descending sequences, i.e.

< descending 1 [] [1, 2]          == [[1],[1,2],[]]

\begin{code}
descending :: Ord a => a -> [a] -> [a] -> [[a]]
descending a as bs = error "Define me!"
\end{code}

3. **(Difficult) Optimize ascending:** Ascending on an increasing list gives us back 
exactly one sequence: 

< ascending' 0 [] [1, 2, 3, 4] == [[0,1,2,3,4],[]]

but to do so, the `ascending'` definition imposes a huge performance leak
as it has to reverse the returning list. 
Define a higher order `ascending` that 
satisfies the below type signature, 
does not reverse the returning sequence and still
behaves like `ascending'` in that 

< forall xs, a: ascending a id xs == ascending' a [] xs  


where `id` is the identity functions. 

**Restrictions:** Increasing lists should be accessed only once! You are not allowed to use `reverse`, `(++)`, 
or any other list operations. Define `ascending` only by using 

- one comparison, 

- recursive calls to `ascending` and `sequences`,

- function application and abstraction (i.e., lambda),

- the list data constructors (`[]` and `(:)`),

- and no intermediate helper functions. 

\begin{code}
ascending :: Ord a => a -> ([a] -> [a]) -> [a] -> [[a]]
ascending a as bs = error "Define me!"
\end{code}

Finally, replace the call to `ascending'` with `ascending` in the `sequences` definition.



**Problem 4:** Tic-Tac-Toe
------------------------------------

- Step 1: Download the [tic-tac-toe](https://github.com/nikivazou/tic-tac-toe) game. 

< git clone https://github.com/nikivazou/tic-tac-toe.git

- Step 2: Install the game and play against the random strategy. 

< cd tic-tac-toe/classic/
< stack install 
< tic-tac-toe

- Step 3: Follow the description in the [src/Player/MinMax.lhs](https://github.com/nikivazou/tic-tac-toe/blob/master/classic/src/Player/MinMax.lhs) file. 

- Step 4: Import and call your player. In [src/TicTacToe.hs](https://github.com/nikivazou/tic-tac-toe/blob/master/classic/src/TicTacToe.hs) add

< import Player.MinMax (playerMinMax)

then in
[src/TicTacToe.hs#L11](https://github.com/nikivazou/tic-tac-toe/blob/master/classic/src/TicTacToe.hs#L11) replace 

< player1 = playerHuman "Me!"

with 

< player1 = playerMinMax

and make sure that your `playerMinMax` always wins the computer. 


Abusing Laziness and HO
------------------------



The following two examples (ab)use Haskell's 
laziness and higher order features to allow 
concise functions of recursive functions.

\begin{code}
module HigherOrder where
import Data.List (nubBy)
\end{code}

- All the prime numbers

\begin{code}
primes :: [Int] 
primes = nubBy (((>1).).gcd) [2..]
\end{code}

- All fibonacci 

\begin{code}
fibs :: [Int] 
fibs = 0:1:(zipWith (+) fibs (tail fibs))
\end{code}

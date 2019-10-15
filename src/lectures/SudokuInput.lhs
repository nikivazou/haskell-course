Sudoku Representation
=====================


\begin{code}
module SudokuInput where 
\end{code}


Sudoku Examples
---------------------------

Here is a Sudoku puzzle: 

\begin{code}
example1 
 = [
    "-------------",    
    "|2..|..1|.38|",
    "|...|...|..5|",
    "|.7.|..6|...|",
    "|-----------|",
    "|...|...|.13|",
    "|.98|1..|257|",
    "|31.|...|8..|",
    "|-----------|",
    "|9..|8..|.2.|",
    "|.5.|.69|784|",
    "|4..|25.|...|",
    "-------------" 
    ]
\end{code}

and a smaller one: 

\begin{code}
example0 
  = [
    "-------",
    "|43|..|", 
    "|12|3.|",
    "-------",
    "|..|2.|",
    "|21|..|",
    "-------"
    ]
\end{code}

**Q:** What is the type of a Sudoku? 

Remove Redundant Info
---------------------------

1. Let's remove the `--...` lines:

\begin{code}
rmLines :: [[Char]] -> [[Char]]
rmLines [] = []
rmLines (x:xs)
  | elem '-' x = rmLines xs 
  | otherwise  = x:rmLines xs 
\end{code}

2. Let's remove the `|`` from lines:
\begin{code}
rmSep :: [Char] -> [Char] 
rmSep [] = []
rmSep (x:xs) 
  | '|' == x = rmSep xs 
  | otherwise = x:rmSep xs  
\end{code}

3. Programming Patterns: Can you spot the difference?
\begin{code}
rm :: (a -> Bool) -> [a] -> [a] 
rm p [] = []
rm p (x:xs) 
  | p x       = rm p xs 
  | otherwise = x:rm p xs

rmLines' :: [[Char]] -> [[Char]]  
rmLines' xs  = rm p xs 
  where p x = elem '-' x

rmSep' :: [Char] -> [Char] 
rmSep' xs = rm p xs
  where p x = '|' == x  
\end{code}

4. Use Lib Functions!!!!!

\begin{code}
rmLines'' :: [[Char]] -> [[Char]]  
rmLines'' xs  = filter p xs 
  where p x = notElem '-' x

rmSep'' :: [Char] -> [Char]  
rmSep'' xs = filter p xs
  where p x = '|' /= x 
\end{code}

5. How do I apply `rmSep` to all lines? 

\begin{code}
rmSepAll :: [[Char]] -> [[Char]]
rmSepAll [] = []
rmSepAll (x:xs) = rmSep'' x:rmSepAll xs 
\end{code}

Recursion is **BAD**!!!!
Let's remove it: 

\begin{code}
rmSepAll' :: [[Char]] -> [[Char]]
rmSepAll' = map rmSep 
\end{code}


FINALLY: Put them all together

\begin{code}
clean' :: [[Char]] -> [[Char]]
clean' xs = map rmSep (rmLines xs)
\end{code}


Using Diagrams
----------------------

clean DIAGRAM:

\begin{spec}
    [
    "-------",
    "|43|..|", 
    "|12|3.|",
    "-------",
    "|..|2.|",
    "|21|..|",
    "-------"
    ]
       ||
rmLines ::[[Char]] -> [[Char]]
       ||
       \/ 
    [
    "|43|..|", 
    "|12|3.|",
    "|..|2.|",
    "|21|..|",
    ]

       ||
map rmSep ::[[Char]] -> [[Char]]
       ||
       \/ 
    [
    "43..", 
    "123.",
    "..2.",
    "21..",
    ]
\end{spec}

DIAGRAM SUMMARY: 
\begin{spec}
clean         :: [[Char]] -> [[Char]]
 1. rmLines   :: [[Char]] -> [[Char]]
 2. map rmSep :: [[Char]] -> [[Char]]
\end{spec}

Compositional Programming! 

\begin{code}
clean'' :: [[Char]] -> [[Char]]
clean'' = map rmSep . rmLines
\end{code}




SUMMARY 
--------

1. Use library function (i.e., hoogle)! 
2. Higher Order Programming Patterns, e.g., filter, map
3. Compositional Programming 
   a) depompose problem to subprobles
   b) solve them independently 
   c) solution is composition of solutions! 

Apply these steps to [solve this sudoku](lectures/SudokuSolver.html)!
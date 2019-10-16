module SudokuInput where 

-----------------------------------
-- WarmUp: Sudoku Examples --------
-----------------------------------

-- Here is a Sudoku puzzle: 

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

-- and a smaller one: 

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

-- **Q:** What is the type of a Sudoku? 

---------------------------
-- Remove Redundant Info -- 
---------------------------

-- 1. Let's remove the "-- lines":

rmLines :: [[Char]] -> [[Char]]
rmLines = undefined 













-- 2. Let's remove the "|" from lines:
rmSep :: [Char] -> [Char] 
rmSep = undefined 










-- 3. Programming Patterns: Can you spot the difference?













-- 4. Use Lib Functions!!!!!














-- 5. How do I apply rmSep to all lines? 
rmSepAll :: [[Char]] -> [[Char]]
rmSepAll = undefined 














-- Hope you did not use recursion....











-- FINALLY: Put them all together















{- clean DIAGRAM:

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

DIAGRAM SUMMARY: 
clean         :: [[Char]] -> [[Char]]
 1. rmLines   :: [[Char]] -> [[Char]]
 2. map rmSep :: [[Char]] -> [[Char]]
-}

clean' :: [[Char]] -> [[Char]]
clean' = map rmSep . rmLines

-- Compositional Programming! 



---------------------------------
-- SUMMARY ----------------------
---------------------------------

-- 1. Use library functions &  hoogle! 
-- 2. Higher Order Programming Patterns, e.g., filter, map
-- 3. Compositional Programming 
--   a) depompose problem to subprobles
--   b) solve them independently 
--   c) solution is composition of solutions! 


-- Apply these steps to solve this sudoku!
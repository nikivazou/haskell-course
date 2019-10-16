module SudokuSolver where 


{- solve DIAGRAM

   ["43..", 
    "123.",
    "..2.",
    "21.."]

       ||
solve ::[[Char]] -> [[Char]]
       ||
       \/ 

   ["4312", 
    "1234",
    "3421",
    "2143"]

Break to subproblems! 


   ["43..", 
    "123.",
    "..2.",
    "21.."]

       ||
choices :: [[Char]] -> [[[Char]]]
       ||
       \/ 

   [[[4],[3],[1,2,3,4],[1,2,3,4]], 
    [[1],[2],[3],[1,2,3,4]],
    [[1,2,3,4],[1,2,3,4],[2],[1,2,3,4]],
    [[2],[1],[1,2,3,4][1,2,3,4]]

       ||
collapse :: [[[Char]]] -> [[[Char]]]
       ||
       \/ 

  [["4311", ["4311", ["4311", ["4311", ["4311",
    "1231",  "1231",  "1231",  "1231",  "1231",
    "1121",  "1121",  "1121",  "1121",  "1121",
    "2111"], "2112"], "2113"], "2114"], "2121"],... 

       ||
filter valid :: [[[Char]]] -> [[[Char]]]
       ||
       \/ 

  [["4312", 
    "1234",
    "3421",
    "2143"]]

DIAGRAM SUMMARY: 
solve            :: [[Char]] -> [[Char]]
 1. choices      :: [[Char]] -> [[[Char]]]
 2. collapse     :: [[[Char]]] -> [[[Char]]]
 3. filter valid :: [[[Char]]] -> [[[Char]]]
-}

-- Problem 1: Is Char a choice or a value?











-- Problem 2: Types don't really compose! 















{- 
DIAGRAM SUMMARY: 
solve            :: Sudoku -> Sudoku
 1. choices      :: Sudoku -> Matrix Choice 
 2. collapse     :: Matrix Choice -> [Matrix Value]
 3. filter valid :: [Matrix Value] -> [Matrix Value]
 4. ??           :: [Sudoku] -> Sudoku
-}










-- 1. define choices 













-- 2. define collapse 

{- collapse DIAGRAM

   [[[1,2],[3,4]], 
    [[1],[2,3]]  ]

       ||
collapse ::[[[Char]]] -> [[[Char]]]
       ||
       \/ 

   [ [[1,3]  [[1,3]  [[1,4]  [[1,4]  [[2,3]  [[2,3]  [[2,4]  [[2,4]
     ,[1,2]], [1,3]], [1,2]], [1,3]], [1,2]], [1,3]], [1,2]], [1,3]]]

Break to subproblems! 

   [[[1,2],[3,4]], 
    [[1],[2,3]]  ]

       ||
map prod ::[[[Char]]] -> [[[Char]]]
       ||
       \/ 

   [[[1,3],[1,4],[2,3],[2,4]], 
    [[1,2],[1,3]]            ]

       ||
prod ::[[[Char]]] -> [[[Char]]]
       ||
       \/ 
   [ [[1,3]  [[1,3]  [[1,4]  [[1,4]  [[2,3]  [[2,3]  [[2,4]  [[2,4]
     ,[1,2]], [1,3]], [1,2]], [1,3]], [1,2]], [1,3]], [1,2]], [1,3]]]
 
DIAGRAM SUMMARY: 
collapse       :: Martix Choices -> [Matrix Value]
1. map prod    :: [[[Char]]] -> [[[Char]]]
2. prod        :: [[[Char]]] -> [[[Char]]]

-}







-- 2a. Did you test collapse? 













-- 2: Define Valid 
-- 2a: Let's start from rows 
















-- 2b: Continue with columns... 
--     Maybe ask hoogle directly...

cols :: [[a]] -> [[a]]
cols = undefined 












-- 2c: Ok, boxes are more complicated...

boxsize :: Int 
boxsize = 3 

boxs :: [[a]] -> [[a]]
boxs = unpack . map cols . pack
  where
  pack   = split . map split
  split  = chop boxsize
  unpack = map concat . concat

chop  :: Int -> [a] -> [[a]]
chop n []  =  []
chop n xs  =  take n xs : chop n (drop n xs)

{- 
Example: if boxsize = 2, then we have 

   [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15,16]]

                          |
                         pack
                          |
                          v

   [[[[1,2],[3,4]],[[5,6],[7,8]]],[[[9,10],[11,12]],[[13,14],[15,16]]]]

                          |
                       map cols
                          |
                          v

   [[[[1,2],[5,6]],[[3,4],[7,8]]],[[[9,10],[13,14]],[[11,12],[15,16]]]]

                          | 
                        unpack
                          |
                          v

   [[1,2,5,6],[3,4,7,8],[9,10,13,14],[11,12,15,16]]
-}













-- Let's try the small one! 












-- Before we go big, let's see the choises first... 













-- Ok, rows can be obsviously pruned! 
pruneRows = undefined 












-- Let's generalize pruning ... 




















-- Oups! Too slow... 












-- Can we prune more?











-- Still too slow... 
-- The problem: it should backtrack: 
-- Bad choices are first fully expanded and then checked for validity
-- We need to expand one choise at the time


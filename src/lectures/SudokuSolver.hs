module SudokuSolver where 

import Data.List
import Constants 

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

type Sudoku = Matrix Value 
type Matrix a = [[a]]

type Choice = [Char]
type Value  = Char 







-- Problem 2: Types don't really compose! 












{- 
DIAGRAM SUMMARY: 
solve            :: Sudoku -> Sudoku
 1. choices      :: Sudoku -> Matrix Choice 
 2. collapse     :: Matrix Choice -> [Matrix Value]
 3. filter valid :: [Matrix Value] -> [Matrix Value]
 4. ??           :: [Sudoku] -> Sudoku
-}



solve :: Sudoku -> [Sudoku]
solve = filter valid . collapse . choices 
    


--------------------------------------
choices :: Matrix Char -> Matrix [Char] 
--------------------------------------
choices = map (map toChoices)

toChoices :: Char -> [Char]
toChoices '.' = values 
toChoices x   = [x]



--------------------------------------
collapse :: Matrix Choice -> [Matrix Value]
--------------------------------------

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

collapse = prod . map prod

prod :: [[a]] -> [[a]]
prod [] = [[]]
prod (xs:xss) = [y:ys | y <- xs, ys <- prod xss]



--------------------------------------
valid :: Matrix Value -> Bool 
--------------------------------------
valid xs =  all nodups (rows xs)
         && all nodups (cols xs)
         && all nodups (boxs xs)


nodups :: Eq a => [a] -> Bool 
nodups []     = True 
nodups (x:xs) = notElem x xs && nodups xs 


rows :: Matrix a -> [[a]]
rows xs = xs  

cols :: [[a]] -> [[a]]
cols = transpose   

boxs :: Matrix a -> [[a]]
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


-- Let's try it

trysmall = solve sudokusmall 

trybig   = undefined 

-- Wait! Let's see the choises first... 

choicesBig = choices sudoku





-- Rows can be obsviously pruned! 
pruneRows = undefined 












-- Let's generalize pruning ... 
pruneAll :: Matrix Choice -> Matrix Choice
pruneAll = pruneBy rows . pruneBy cols . pruneBy boxs
  where pruneBy f = f . map prune . f

prune :: [[Char]] -> [[Char]]
prune xs = map remove xs
  where singles :: [Char]
        singles = concat $ filter single xs 
        remove :: [Char] -> [Char]
        remove [x] = [x]
        remove ys = filter (`notElem` singles) ys  

single :: [a] -> Bool 
single [_] = True 
single _   = False 


solve1 :: Sudoku -> [Sudoku]
solve1 = filter valid . collapse . pruneAll . choices 

-- Oups! Too slow... 












-- Can we prune more?





solve2 :: Sudoku -> [Sudoku]
solve2 = filter valid . collapse . fix pruneAll . choices  

fix f x =  if x == x' then x else fix f (f x)
  where x' = f x 















-- Still too slow... 
-- The problem: it should backtrack: 
-- Bad choices are first fully expanded and then checked for validity
-- Let's expand one choise at the time




-- Some predicates 

complete :: Matrix Choice -> Bool 
complete = all (all single)

void  :: Matrix Choice -> Bool
void  =  any (any null)

safe    :: Matrix Choice -> Bool
safe cm =  all consistent (rows cm) &&
           all consistent (cols cm) &&
           all consistent (boxs cm)

consistent :: [Choice] -> Bool
consistent =  nodups . concat . filter single

blocked    :: Matrix Choice -> Bool
blocked m  =  void m || not (safe m)


-- Predicate based searching 


search                :: Matrix Choice -> [Sudoku]
search m
  | blocked m          =  []
  | complete m         =  collapse m
  | otherwise          =  [g | m' <- expand m
                             , g  <- search (pruneAll m')]

-- Where expand only opens one choice at the time! 


expand                :: Matrix Choice -> [Matrix Choice]
expand m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any (not . single)) m
      (row1,cs:row2)    = break (not . single) row


-- And finally a quick solution

solve3 :: Sudoku -> [Sudoku]
solve3 =  search . pruneAll . choices







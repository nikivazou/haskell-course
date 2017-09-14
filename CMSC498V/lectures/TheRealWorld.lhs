Haskell in the Real World
===========

Let's look how a real Haskell program looks like
by examining the tic-tac-toc game in [https://github.com/nikivazou/tic-tac-toe](https://github.com/nikivazou/tic-tac-toe).

Basic build
----------

We can build the game by running 

< cd src 
< ghc --make TicTacToe.hs

Then the executable `TicTacToe` is created. 
This executable runs the function `main :: IO ()` inside the module `Main`.
Usually, the Main module mostly interacts with the real world, 
while all the pure functions and types are defined in different modules. 
Here, all the types are defined in `Types` and all the playing strategies in the `Strategies.*`. 

Modules
-----------------

In Haskell each file is a module.
Each module defines a set of functions (that other modules can import). 

A module can export all the defined functions, e.g., 

< module Types where 

or some specific functions, e.g., 

< module Player.Computer (playerComputer) where 

When importing modules you again have many alternatives: 

- Import all the functions in the module

< import Types 

- Import specific functions 

< import Player.Human (playerHuman)

- Hide some functions (why?)

< import Prelude hiding ((!!))

- Import qualified module, where you have you call the functions using all the 
name, e.g., `Data.List.intercalate` (why?)

< import qualified Data.List 

- Import qualified module, but with a name alias, e.g., `M.fromJust`.

< import qualified Data.Maybe as M 


Type Definitions
----------------

Two ways to define types in Haskell are `data` and `type`. 
We already saw that `data` defines **new** data types, e.g., 

< data Tile = EmptyTile | X | O 

On the other hand, type `type` is used to define type alias for existing types (why?). 
For example a move is a pair of integers, 

< type Move   = (Int,Int)

while the board maps moves to tiles

< type Board  = [(Move, Tile)]

Data and types can get quite interesting! 
For example, the `Types.Player` data type has a function 
`playerMove` that defines all the functionality of the tic-tac-toe game. 

< data Player = 
<  Player { playerMove :: Tile -> Board -> IO Move
<         , playerName :: String
<         }

`Player.Human` and `Player.Computer` define different values of this data type, 
by defining how a human and the computer play a move. 


Deriving instances 
-------------------

Haskell supports overloading of functions 
(that is the same function performs different operations depending on the arguments
that it is called) via type classes. 

Two of the most famous type classes are `Eq` and `Show` for equality checking and 
printing resp. 

< class Eq a where 
<    (==) :: a -> a -> Bool 

< class Show a where
<   show :: a -> String 

There are two ways to define the `(==)` and `show` methods for a data type

- Instance declaration (manually write an instance)

< instance Show Tile where
<   show EmptyTile = "     "
<   show X         = "  X  "
<   show O         = "  O  "

- Deriving Annotation (let the compiler derive it for you)

< data Tile = EmptyTile | X | O 
<  deriving (Eq)

Why not always automatically derive the instances? 

1. Sometimes the derived instance is not the desired one. 
See what happens when the `show` method for `Tile` is derived. 

2. Sometimes derivation is *impossible*. 
For example, the compiler cannot derive a `show` for `Players` 
as there is no (standard) way to show functions. But we can define show
just to print `playerName`. 


Package Manager
----------------

One can compile applications directly with ghc, 
but as the application grows we really need a package manager. 

- Portability. Even tic-tac-toe depends on a library [`random`](https://hackage.haskell.org/package/random-fu-0.2.7.0/docs/Data-Random.html). What version of the library is compatible with our application?

- Distribution. Assuming I know the correct version of `random` then before to compile tic-tac-toc I first need to install `random`. 

- Common interface. If I build a library (like random) how can users know what this library is about/where to find my library. 

Haskell's package manager is [`cabal`](https://www.haskell.org/cabal/). 

- Each application needs a `.cabal` file that (semi-automatically) specifies the dependencies and further documentation.

< # tic-tac-toe.cabal
< build-depends: random >=1.1 && <1.2

**Q:** Why do we need lower and upper bounds?

- Cabal will install all the dependencies for you by searching the Haskell packages on [Hackage](http://hackage.haskell.org/). Anyone can submit packages to Hackage (as long as they have a cabal file).

< cabal install 

What if I simultaneously want to build two different application with conflicting dependencies? 

< # evil-tic-tac-toe
< build-depends: random >=0.6 && <1.0

Sandboxing lets you have locally different versions of the same library specific for each application you are building. Haskell's sandboxer is [stack](https://docs.haskellstack.org/en/stable/README/). It requires a (semi-automatically built) `stack.yaml` file and 

< stack init 

will generate a `.stack-work` directory that will store all local dependencies. 

Do not forget the testing
--------------------------

Testing should always be part of the development process!
Cabal lets you build a test suite. Here 

< cabal configure --enable-tests
< cabal test 

will run `tests/Test.hs` (that indeed does nothing...).
Alternatively, just run

< stack test


Code sharing 
------------

When your project is ready, share it on Hackage so that other Haskellers enjoy it! 

Until it is ready upload it on [github](https://github.com). Why?

- Version control will answer many questions
    - what was that edit I made yesterday when my code was fast/worked?
    - who broke/fixed my code?
    - why did I make this edit? (assuming good commit messages...)

- Test integration: [Travis](https://travis-ci.org/) (assuming you manage to set it correct) runs unit test at all your commits!  


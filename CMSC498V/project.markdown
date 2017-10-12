The project will happen at the *second* half of the semester. 
The goal of the project is to give you some realistic experience of Haskell development and [pair programming](https://en.wikipedia.org/wiki/Pair_programming).
You are expected to work in pairs and spend at least *30 hours* in the project. 

- *Tue, 10/10:* Project Registration
- *Tue, 10/17:* Project Proposal 
- *Tue, 11/07:* Project Updates
- *Tue, 12/05:* Project Presentation 

Project Ideas
--------------

You can implement any Haskell project you wish. If you think your idea is unrealistic, discuss it with the instructor or the TA. 
Here are some ideas: 

- Games 
	- A strategy game against the human, e.g., chess. 
	- A puzzle solver, e.g., sudoku.
	- A real game using Functional Reactive Programming ([haskanoid](https://github.com/ivanperez-keera/haskanoid)).
- Applications	
	- A web or database application ([Yesod](https://www.yesodweb.com/)). 
	- A Machine Learning implementation.
- Libraries 
	- An efficient implementation of a data structure, e.g., string matching, red-black trees. 
	- Any library you think is missing from the Haskell ecosystem. 
- Theory and Logic 
	- A parser, type-checker, and/or interpreter for lambda-calculus. 
	- A prover of propositional logic. 
	- [Liquid Haskell:](https://ucsd-progsys.github.io/liquidhaskell-blog/)Correctness of [bird-style transformations](https://github.com/nikivazou/nikivazou.github.io/blob/master/static/projects/bird-style-equivalence.md).

Project Evaluation
------------------

The project will be evaluated based on the following criteria: 

- *collaboration:* the project will be built on [github](https://github.com/) and all members should contribute equally. 
- *library usage:* the project should use existing libraries of the [hackage](https://hackage.haskell.org/) ecosystem.  
- *reproduction:* the project should be easily installed via [cabal](https://www.haskell.org/cabal/) or [stack](https://docs.haskellstack.org/en/stable/README/).
- *unit testing:* the project should pass unit tests that are implemented either manually or using [quickcheck](https://hackage.haskell.org/package/QuickCheck). 
The tests should be automatically checked via [Travis CI](https://travis-ci.com/) or [Circle CI](https://github.com/marketplace/circleci).
- *[optional] verification:* optionally you can use [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell-blog/) to check validity of your code.
- *presentation:* make sure to clearly present your goals and your implementation.


*Tue, 10/10:* Project Registration
----------------------------------

You need to create a `github` repository for your project, write a `Readme.md` file that describes your goals and write *your names* and the *github link* to the [this](https://docs.google.com/document/d/1OQCYmsOTK25uycGGNqQ_6hyJPvKBxSNudDiNxUjQ8RA/edit?usp=sharing) document.

*Tue, 10/17:* Project Proposal 
-------------------------------

You will give a *5 min* description of your project. 

*Tue, 11/07:* Project Updates
-----------------------------

You will give a *5 min* description of your progress: 

- What challenges (if any) did you have so far and how did you solve them?
- Do you expect to meet your goals until the deadline?
- How are your goals adapted?

*Tue, 12/05:* Project Presentation 
-----------------------------------

You will give a *10 min* description of your project.

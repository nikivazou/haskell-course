Tournament
===========


For the tournament you are requested to implement a strategy 
that solves [gomoku](https://en.wikipedia.org/wiki/Gomoku), 
a generalization of tic-tac-toe. 

You should submit your implementation any time on **17/11**. 


Game Rules 
-----------

- The game is played on a **15x15** board.
- The tile of the first player is X and the tile of the second is O.
- Each player has **30 sec** to decide their move. 
- Players alternate turns placing their tile on an empty intersection. 
- The winner is the first player to form an unbroken chain of **five** tiles horizontally, vertically, or diagonally.


How to play
------------

- Pick up a name for your team. In the following steps replace the world `Lambda` with your team name. 

- Clone the tic-tac-toe directory and navigate in the `gomoku` folder. 

```
git clone https://github.com/nikivazou/tic-tac-toe.git
cd tic-tac-toe/gomoku/
```

- Create the file that implements your strategy. 

```
touch src/Player/Lambda.hs
```

- Initiate the file you just created as below:

```
module Player.Lambda (playerLambda) where

import Types

teamMembers :: String 
teamMembers = "Write your name to get the tournament points"

playerLambda :: Player 
playerLambda = error "Define me!"
```

**Note:** In the above initiation you should only change `Lambda` with your team name. 
It is important that your team-name starts with a capital letter, so that it can be a module name. 

- Replace the call to `error` with your implementation. 

- Register your team by running

```
sh makeplayers.sh
```

The above will output 

```
# Players registerd:
# BestNext
# Computer
# Human
# Lambda
```

Check that your team is indeed added as a registered players. 

- Install `gomoku`

```
stack install
```

- Play by calling `gomoku` with exactly two team-names

```
gomoku Lambda Computer
```

How to submit 
-------------

To submit your strategy, generate a [Pull Request](https://help.github.com/articles/creating-a-pull-request/) to the tic-tac-toc repository
that adds your `Strategy.Lambdas.hs` file and does not touch anything else. 

Alternatively, you can send your file to the instructor by email, with subject "Haskell-Course: Tournamet".

You can submit multiple pull requests as long as you make sure that by the end of the deadline 
you have only one team submitted.
You can only send one email. 




Grading
-------

- If you do not submit a file or your file does not compile (please do not...) you get 0 points. 
- If your strategy makes more than 30 sec or crashes you get half points. 
- Otherwise, you get full points. 

Teams and Ties
--------------

The opponents will be decided by a random programs exactly before class. 
Each game will be 5 rounds of gomoku. 
In case of a tie, a coin will be tossed. 

Integrity,  Privacy and Copyrights. 
-----------------------------------

Sending your solution via a pull request has two benefits

- It makes setting up the tournament easy. 
- You will learn how to create a pull request.

But also it will make your code public. 
If you do not want your code to get public, send your solutions via email to the instructor.

Making your solutions public means that some students 
can copy and submit your solution. 
We rely on your integrity and respect for your classmates 
on not copying or looking at other solutions, before you submit yours. 
Yet, feel free to test your code against other submitted solutions. 
Or even submit "testing" solutions: solutions without the `teamMembers`
that can be used only for testing. 

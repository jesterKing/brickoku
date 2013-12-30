Brickoku
========

This is code related to the Brickoku game, an orginal puzzle game developed by 
Nathan Letwory.

The game is to build a brick model based on 10 numbers. The numbers encode each
brick in a certain way. Using a simple set of rules the player can determine
what model hidden in the 10 numbers.

Number and a brick
------------------

For the game a simple 2x2 brick is used. Looking from the top with a side of a
brick oriented to your body the connection points are number from bottom-left
to bottom-right, top-left to top-right: 1 2 3 4. The bottom connection points
are numbered the same: bottom connection 1 is in the same corner as top
connection 1.

	3    4
	
	1    2

The bit representation of a number will have 14 bits used. The lower 7 bits are
used for the bottom part of a brick, the upper 7 bits for the top part of a
brick. The 3 most significant bits encode the amount of bricks that are
connected to the top of a brick. The next 4 bits encode which connection points
are connected. Of these four bits the most significant bit is the highest index
(4), the lowest significant bit the lowest index (1). The following four bits
encode which connection points on the bottom are connected, and of the entire
number the 3 least significant bits are used to encode the number of bricks
that are used to connect on the bottom.

So the form is:

> 000 0000 0000 000 (top count, top connections, bottom connections, bottom count)

Two bricks right on top of each other will thus yield two bitstrings:

> 001 1111 0000 000 (3968d)
>
> 000 0000 1111 001 (121d)

Note that if you rotate a model 90 degrees you'll end up with different
encoding, since the position encoding is always with respect to the gamer. So
if you have a brick half on top to the right side of the lower brick you'd
have:

> 001 1010 0000 000 (3328d)
>
> 000 0000 0101 001 (41d)

But if you rotate the model 90 degrees counter-clockwise you end up with:

> 001 1100 0000 000 (3584d)
>
> 000 0000 0011 001 (25d)

Challenge
---------

With this information you should be able to solve the puzzle with the following
10 numbers:

<b>122 2769 2769 2809 3369 3369 3449 4009 4049 6016</b>



Software Features
=================

* Generate models
* Output brick locations
* Generate numbers for bricks in a model
* Generate bit strings for bricks in a model

Roadmap
-------

* Solver for collection of numbers
* Direct visualisation
* Game mode

Compiling
=========

ghc -o brickoku-game -O -threaded -rtsopts brickoku-game.hs

Usage
=====

	./brickoku-game [[[seed] brick_count] count] [+RTS rtsopts]

When `seed` is not specified a random seed will be picked. `count` tells how
many model iterations will be created.

### a random model with 10 bricks

	./brickoku-game

### 5 models with a random seed, 10 bricks

	./brickoku-game 5

### 1 model with a specific seed, 10 bricks

	./brickoku-game 1337 10 1

### 5 models with a specific seed, 20 bricks

	./brickoku-game 7331 20 5

### start with 3 threads, extra output

	./brickoku-game 7331 20 5 +RTS -sstderr -N3

Credits
=======

Teemu Paukku - valuable help with the first steps in creating the game on paper 
and using lego bricks.

Further
=======

This project serves as a personal learning platform for the Haskell
language.



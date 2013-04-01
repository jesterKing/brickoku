Brickudo
========

This is code related to the Brickudo game, an orginal puzzle game developed by 
Nathan Letwory.

This project also serves as a personal learning platform for the Haskell language.

Compiling
=========

ghc -o brickudo-game -O brickudo-game.hs

Usage
=====

	./brickudo-game [[seed] count]

When `seed` is not specified a random seed will be picked. `count` tells how many model iterations will be created.

## a random model

	./brickudo-game

## 5 models with a random seed

	./brickudo-game 5

## 1 model with a specific seed

	./brickudo-game 1337 1

## 5 models with a specific seed

	./brickudo-game 7331 5

Credits
=======

Teemu Paukku - valuable help with the first steps in creating the game on paper 
and using lego bricks.

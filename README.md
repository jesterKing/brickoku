Brickodu
========

This is code related to the Brickodu game, an orginal puzzle game developed by 
Nathan Letwory.

This project also serves as a personal learning platform for the Haskell language.

Compiling
=========

ghc -o brickodu-game -O brickodu-game.hs

Usage
=====

	./brickodu-game [[seed] count]

When `seed` is not specified a random seed will be picked. `count` tells how many model iterations will be created.

## a random model

	./brickodu-game

## 5 models with a random seed

	./brickodu-game 5

## 1 model with a specific seed

	./brickodu-game 1337 1

## 5 models with a specific seed

	./brickodu-game 7331 5

Credits
=======

Teemu Paukku - valuable help with the first steps in creating the game on paper 
and using lego bricks.

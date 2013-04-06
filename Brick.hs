module Brick ( models, renderModel, vectors, numbers, bitStrings ) where

import Data.Set ( fromList, toList, difference, intersection )
import Data.List ( intercalate, sort )
import Data.Bits ( shift, testBit )
import Data.Int ( Int16 )
import System.Random ( mkStdGen, StdGen, randomR, next )

-- data type holding coordinates
data Location = Location {x :: Int, y :: Int, z :: Int}
	deriving (Show, Ord, Eq)

-- data type representing a brick and its origin
data Brick = Brick { loc :: Location }
	deriving (Show, Ord, Eq)

-- a model is a collection of bricks that are connected
type Model = [Brick]

-- base locations contain X and Y of brick openings
-- from these connection points are calculated. These tuples also
-- contain values that are used to calculate the number for a brick in a
-- Model.
_baseLocations :: [(Int, Int, Int)]
_baseLocations = [
	(-1, 1, 4),
	(0, 1, 12),
	(1, 1, 8),
	(-1, 0, 5),
	(0, 0, 15),
	(1, 0, 6),
	(-1, -1, 1),
	(0, -1, 3),
	(1, -1, 2)
	]


-- create locations list
_makeConnectionPoints :: Int -> Brick -> [Location]
_makeConnectionPoints n brick = map (\(a, b, _) -> Location (a+x') (b+y') (n+z')) _baseLocations 
	where
		x' = x ( loc brick )
		y' = y ( loc brick )
		z' = z ( loc brick )

-- Give a list of connection points on the top of a brick
_topConnectionPoints :: Brick -> [Location]
_topConnectionPoints brick = _makeConnectionPoints 2 brick


-- Give a list of connection points on the bottom of a brick
_bottomConnectionPoints :: Brick -> [Location]
_bottomConnectionPoints brick = _makeConnectionPoints (-2) brick


-- give a list of all connection points for a brick
_connectionPoints :: Brick -> [Location]
_connectionPoints brick = _topConnectionPoints brick ++ _bottomConnectionPoints brick


-- Give a list of locations that cannot be occupied
_blockLocations :: Brick -> [Location]
_blockLocations brick = _makeConnectionPoints 0 brick


-- Give the list _origins for a list of bricks
_origins :: Model -> [Location]
_origins [] = []
_origins bs = [loc b | b <- bs]

-- Give all the potential locations around given bricks
_modelLocations :: Model -> [Location]
_modelLocations [] = []
_modelLocations (x:xs) = _modelLocations xs ++ _connectionPoints x

-- Give all the blocking locations of given bricks
_modelBlockLocations :: Model -> [Location]
_modelBlockLocations [] = []
_modelBlockLocations (x:xs) = _modelBlockLocations xs ++ _blockLocations x

-- Return a random location from a list of locations
_pickConnection :: StdGen -> [Location] -> (Location, StdGen)
_pickConnection rgen [] = (Location 1 1 1 , rgen)
_pickConnection rgen xs = (xs !! pos , rgen2)
	where
		(pos, rgen2) = randomR (0, length xs-1) rgen

-- Pick the next brick for Model.
_nextBrick :: StdGen -> Model -> (Brick, StdGen)
_nextBrick rgen m = (brick, rgen2)
	where
		block = _modelBlockLocations m
		blockS = fromList block
		all = _modelLocations m
		allS = fromList all
		availableL = toList $ difference allS blockS
		(location, rgen2) = _pickConnection rgen availableL
		brick = Brick location

-- Grow Model with the next brick.
_modelNextBrick :: StdGen -> Model -> (Model, StdGen)
_modelNextBrick rgen m = (brick:m, rgen2)
	where
		(brick, rgen2) = _nextBrick rgen m

-- Monad for creating a series of models
_model' :: StdGen -> Model -> Int -> Model
_model' rgen [] n = _model' rgen [Brick $ Location 1 1 1] (n - 1)
_model' rgen m n = do
	let (m1, ngen) = _modelNextBrick rgen m
	let n' = n - 1
	if n' > 0
		then
			_model' ngen m1 n'
		else
			reverse m1

-- Simple helper for translating a Brick
_translate :: Brick -> (Int, Int, Int) -> Brick
_translate b (tx,ty,tz) = Brick $ Location ((x $ loc b) - tx) ((y $ loc b) - ty) ((z $ loc b) - tz)

-- Move Model to the first quadrant (all values >= 0)
_toFirstQuadrant :: Model -> Model
_toFirstQuadrant [] = []
_toFirstQuadrant m = [_translate brick trans | brick <- m]
	where
		ls = vectors m
		trans = (minimum [x|(x,_,_) <- ls], minimum [y|(_,y,_) <- ls],
						minimum [z|(_,_,z) <- ls])

-- Part of number for brick in Model
_numberPart :: Brick -> (Brick -> [Location]) -> Model -> (Int, Int)
_numberPart _ _ [] = (0, 0)
_numberPart b f m = (t, c)
	where
		ml = fromList $ _origins m
		cp = fromList $ f b
		bricks = toList $ intersection ml cp
		c = length bricks
		t = sum [s | tl <- bricks, (x1, y1) <- [(x tl, y tl)],
				(x', y') <- [(x $ loc b, y $ loc b)], (lx, ly, s) <- _baseLocations, lx ==(x1-x') && ly == (y1-y')]

-- Number for a Brick in Model
_numberBrick :: Brick -> Model -> Int16
_numberBrick _ [] = 0
_numberBrick b m = fromIntegral ( (shift topcount 11) + ( shift top 7 ) + ( shift bottom 3 ) + bottomcount )
	where
		(top, topcount) = _numberPart b _topConnectionPoints m
		(bottom, bottomcount) = _numberPart b _bottomConnectionPoints m


-- Generate c models of n bricks into list m of Models
models :: StdGen -> Int -> Int -> Model -> [Model]
models rgen count brickcount ms
	| count <= 0 = []
	| otherwise = [_toFirstQuadrant $ _model' rgen [] brickcount] ++ models rgen2 (count-1) brickcount ms
		where
			(x, rgen2) = next rgen


-- Generate a string representation of a Model
renderModel :: Model -> String
renderModel m = "[\n" ++ bricks m ++ "\n]"
	where
		bricks [] = ""
		bricks ls = intercalate ",\n" (map renderBrick ls)
		renderBrick b = "\t(" ++ ( show $ x $ loc b ) ++ ", " ++ ( show $ y $ loc b ) ++ ", " ++ ( show $ z $ loc b ) ++ ")"


-- Generate numbers of a Model
numbers :: Model -> [Int16]
numbers [] = []
numbers m = [ _numberBrick b m | b <- m ]


-- Generate a bitstring representation of an Int16
bitString :: Int16 -> String
bitString a = intercalate "" [ if testBit a x then "1" else "0" | x <- [13,12..0]]


-- Generate bitstrings for a Model
bitStrings :: Model -> [String]
bitStrings [] = []
bitStrings m = [ bitString i | i <- numbers m ]


-- Vector tuples for a Model
vectors :: Model -> [(Int, Int, Int)]
vectors [] = []
vectors m = [(x l, y l, z l) | l <- _origins m]

gen = mkStdGen(1)
ms = models gen 100 10 []
m = ms !! 98
--b = m !! 5

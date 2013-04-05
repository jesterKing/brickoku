module Brick ( models', origs, renderModel, vectorsModel, numbersModel, bitStringsModel ) where

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

-- helper function that gives a list of locations that
-- are all within the first quadrant
okQuadrant :: [Location] -> [Location]
okQuadrant [] = error "empty list"
okQuadrant xs = [a | a <- xs, z a >= 0] -- && x a >= 0 && y a >= 0]

-- base locations contain X and Y of brick openings
-- from these connection points are calculated
baseLocations :: [(Int, Int)]
baseLocations = [
	(-1, 1),
	(0, 1),
	(1, 1),
	(-1, 0),
	(0, 0),
	(1, 0),
	(-1, -1),
	(0, -1),
	(1, -1)
	]

-- create locations list
makeConnectionPoints :: Int -> Brick -> [Location]
makeConnectionPoints n brick = map (\(a, b) -> Location (a+x') (b+y') (n+z')) baseLocations 
	where
		x' = x ( loc brick )
		y' = y ( loc brick )
		z' = z ( loc brick )

-- Give a list of connection points on the top of a brick
topConnectionPoints :: Brick -> [Location]
topConnectionPoints brick = makeConnectionPoints 2 brick
-- Give a list of connection points on the bottom of a brick
bottomConnectionPoints :: Brick -> [Location]
bottomConnectionPoints brick = makeConnectionPoints (-2) brick

-- give a list of all connection points for a brick
connectionPoints :: Brick -> [Location]
connectionPoints brick = topConnectionPoints brick ++ bottomConnectionPoints brick

-- Give a list of locations that cannot be occupied
blockLocations :: Brick -> [Location]
blockLocations brick = makeConnectionPoints 0 brick


-- Give the list origins for a list of bricks
origs :: Model -> [Location]
origs [] = []
origs bs = [loc b | b <- bs]

-- Give all the potential locations around given bricks
modelLocations :: Model -> [Location]
modelLocations [] = []
modelLocations (x:xs) = modelLocations xs ++ connectionPoints x

-- Give all the blocking locations of given bricks
modelBlockLocations :: Model -> [Location]
modelBlockLocations [] = []
modelBlockLocations (x:xs) = modelBlockLocations xs ++ blockLocations x

-- Return a random location from a list of locations
pickConnection :: StdGen -> [Location] -> (Location, StdGen)
pickConnection rgen [] = (Location 1 1 1 , rgen)
pickConnection rgen xs = (xs !! pos , rgen2)
	where
		(pos, rgen2) = randomR (0, length xs-1) rgen

-- Pick the next brick for Model.
nextBrick :: StdGen -> Model -> (Brick, StdGen)
nextBrick rgen m = (brick, rgen2)
	where
		block = modelBlockLocations m
		blockS = fromList block
		all = modelLocations m
		allS = fromList all
		availableL = toList $ difference allS blockS
		(location, rgen2) = pickConnection rgen availableL
		brick = Brick location

-- Grow Model with the next brick.
modelNextBrick :: StdGen -> Model -> (Model, StdGen)
modelNextBrick rgen m = (brick:m, rgen2)
	where
		(brick, rgen2) = nextBrick rgen m

-- Generate a model of n bricks into list x
model' :: (Num i, Ord i) => i -> StdGen -> Model -> Model
model' n rgen m
	| n < 0 = []
	| otherwise = m ++ model' (n-1) rgen2 (brick:m)
		where
			(brick, rgen2) = nextBrick rgen m


-- Create one Model consisting of 10 bricks
mmodel :: StdGen -> Model
mmodel rgen =
	let
		(m1, rgen1) = modelNextBrick rgen [Brick $ Location 1 1 1] 
		(m2, rgen2) = modelNextBrick rgen1 m1
		(m3, rgen3) = modelNextBrick rgen2 m2
		(m4, rgen4) = modelNextBrick rgen3 m3
		(m5, rgen5) = modelNextBrick rgen4 m4
		(m6, rgen6) = modelNextBrick rgen5 m5
		(m7, rgen7) = modelNextBrick rgen6 m6
		(m8, rgen8) = modelNextBrick rgen7 m7
		(m9, rgen9) = modelNextBrick rgen8 m8
	in m9

-- Monad for creating a series of models
mmodel' :: StdGen -> Model -> Int -> Model
mmodel' rgen [] n = mmodel' rgen [Brick $ Location 1 1 1] (n - 1)
mmodel' rgen m n = do
	let (m1, ngen) = modelNextBrick rgen m
	let n' = n - 1
	if n' > 0
		then
			mmodel' ngen m1 n'
		else
			reverse m1

-- Generate c models of n bricks into list m of Models
models' :: StdGen -> Int -> Int -> Model -> [Model]
models' rgen count brickcount ms
	| count <= 0 = []
	| otherwise = [mmodel' rgen [] brickcount] ++ models' rgen2 (count-1) brickcount ms
		where
			(x, rgen2) = next rgen

-- Generate a string representation of a Model
renderModel :: Model -> String
renderModel m = "[\n" ++ bricks m ++ "\n]"
	where
		bricks [] = ""
		bricks ls = intercalate ",\n" (map renderBrick ls)
		renderBrick b = "\t(" ++ ( show $ x $ loc b ) ++ ", " ++ ( show $ y $ loc b ) ++ ", " ++ ( show $ z $ loc b ) ++ ")"

-- Generate a bitstring representation of an Int16
bitString :: Int16 -> String
bitString a = intercalate "" [ if testBit a x then "1" else "0" | x <- [13,12..0]]

-- helper for encoding locations into numbers
-- tuple first two are for coordinates, the third
-- is the value for the taken spots
locEncode :: [(Int, Int, Int)]
locEncode = [
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

-- part of number for brick in Model
numberPart :: Brick -> (Brick -> [Location]) -> Model -> (Int, Int)
numberPart _ _ [] = (0, 0)
numberPart b f m = (t, c)
	where
		ml = fromList $ origs m
		cp = fromList $ f b
		bricks = toList $ intersection ml cp
		c = length bricks
		t = sum [s | tl <- bricks, (x1, y1) <- [(x tl, y tl)],
				(x', y') <- [(x $ loc b, y $ loc b)], (lx, ly, s) <- locEncode, lx ==(x1-x') && ly == (y1-y')]

numberBrick :: Brick -> Model -> Int16
numberBrick _ [] = 0
numberBrick b m = fromIntegral ( (shift topcount 11) + ( shift top 7 ) + ( shift bottom 3 ) + bottomcount )
	where
		(top, topcount) = numberPart b topConnectionPoints m
		(bottom, bottomcount) = numberPart b bottomConnectionPoints m

-- Generate numbers of a Model
numbersModel :: Model -> [Int16]
numbersModel [] = []
numbersModel m = [ numberBrick b m | b <- m ]

-- Generate bitstrings for a Model
bitStringsModel :: Model -> [String]
bitStringsModel [] = []
bitStringsModel m = [ bitString i | i <- numbersModel m ]

-- Vector tuples for a Model
vectorsModel :: Model -> [(Int, Int, Int)]
vectorsModel [] = []
vectorsModel m = [(x l, y l, z l) | l <- origs m]

--gen = mkStdGen(1)
--ms = models' gen 100 10 []
--m = ms !! 98
--b = m !! 5

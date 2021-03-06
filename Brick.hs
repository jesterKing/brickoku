module Brick ( models, renderModel, vectors, numbers, bitStrings, bschallenge, challenge, sbchallenge, pchallenge, renderPossibles) where


import Debug.Trace

import Data.Set ( fromList, toList, difference, intersection )
import Data.List ( intercalate, sort )
import Data.Bits ( popCount, (.&.), shift, testBit )
import System.Random ( mkStdGen, StdGen, randomR, next )

-- data type holding coordinates
data Location = Location {x :: Int, y :: Int, z :: Int}
	deriving (Show, Ord, Eq)

-- data type representing a brick and its origin
data Brick = Brick { loc :: Location }
	deriving (Show, Ord, Eq)

-- data type representing a brick during solving
data SolverBrick = SolverBrick { id :: Int, number :: Int }
	deriving (Show, Ord, Eq)

type SolverModel = [SolverBrick]

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
_numberBrick :: Brick -> Model -> Int
_numberBrick _ [] = 0
_numberBrick b m = fromIntegral ( (shift topcount 11) + ( shift top 7 ) + ( shift bottom 3 ) + bottomcount )
	where
		(top, topcount) = _numberPart b _topConnectionPoints m
		(bottom, bottomcount) = _numberPart b _bottomConnectionPoints m

-- Test patterns
_testPatterns :: [(Int, [Int])]
_testPatterns = [
	(0, []),
	(1, [8]),
	(2, [4]),
	(3, [12, 8, 4]),
	(4, [2]),
	(5, [10, 8, 2]),
	(6, [4, 2]),
	(7, [12, 10, 8, 4, 2]),
	(8, [1]),
	(9, [1, 8]),
	(10, [5, 4, 1]),
	(11, [12, 8, 5, 4, 1]),
	(12, [3, 2, 1]),
	(13, [10, 8, 3, 2, 1]),
	(14, [5, 3, 4, 2, 1]),
	(15, [15, 12, 10, 8, 5, 4, 3, 2, 1])
	]

-- Return element at i and rest of list
_nxt :: Int -> SolverModel -> (SolverBrick, SolverModel)
_nxt i n = (n !! i, take i n ++ drop (i + 1) n)

-- get the bottom config of a brick
-- fst is positions, snd is count
_bottom :: SolverBrick -> (Int, Int)
_bottom b = ((.&.) (shift (number b) (-3)) 15, (.&.) (number b) 7)


-- get the top config of a brick
-- fst is positions, snd is count
_top :: SolverBrick -> (Int, Int)
_top b = ((.&.) (shift (number b) (-7)) 15, (.&.) (shift (number b) (-11)) 7)


-- helper for match check
_fitmatch :: Int -> Int -> Bool
_fitmatch a b = or [True | (t, ss) <- _testPatterns, x <- ss, t==a, x==b]


-- check if topbrick could fit on top of brick
-- not checking counts
_couldFitOnTop :: SolverBrick -> SolverBrick -> Bool
_couldFitOnTop topbrick brick = w
	where
		-- get bottom config of top brick
		(b, _) = _bottom topbrick
		-- get top config of bottom brick
		(a, _) = _top brick
		w = _fitmatch a b


-- check if bottombrick could fit on bottom of Brick
-- not checking counts
_couldFitOnBottom :: SolverBrick -> SolverBrick -> Bool
_couldFitOnBottom bottombrick brick = w
	where
		(b, _) = _top bottombrick
		(a, _) = _bottom brick
		w = _fitmatch a b

-- Possible fits, fst is top connections, snd is bottom connections
_possible :: SolverBrick -> SolverModel -> (SolverModel, SolverModel)
_possible b n = (tops, bottoms)
	where
		tops = {-trace ("top count: " ++ show ( tcount ) ++ " pop top: " ++ show( poptop) )-} [t | t <- n, _couldFitOnTop t b]
		bottoms = {-trace ("bottom count: " ++ show ( bcount ) ++ "pop bottom: " ++ show (popbottom) )-} [t | t <- n, _couldFitOnBottom t b]
		(tconf, tcount) = _top b
		poptop = popCount tconf
		(bconf, bcount) = _bottom b
		popbottom = popCount bconf


-- Generate c models of n bricks into list m of Models
models :: StdGen -> Int -> Int -> Model -> [Model]
models rgen count brickcount ms
	| count <= 0 = []
	| otherwise = (++) ([_toFirstQuadrant $ _model' rgen [] brickcount]) (models rgen2 (count-1) brickcount ms)
		where
			(x, rgen2) = next rgen


-- Generate a string representation of a Model
renderModel :: Model -> String
renderModel m = "[\n" ++ bricks m ++ "\n]"
	where
		bricks [] = ""
		bricks ls = intercalate ",\n" (map renderBrick ls)
		renderBrick b = "\t(" ++ ( show $ x $ loc b ) ++ ", " ++ ( show $ y $ loc b ) ++ ", " ++ ( show $ z $ loc b ) ++ ")"

-- Generate a string representation of possible fits
renderPossibles :: [(SolverModel, SolverModel)] -> String
renderPossibles xX = "" ++ potentials xX ++ "\n"
	where
		potentials [] = ""
		potentials ls = intercalate ",\n" (map renderPotential ls)
		renderPotential (t, b) = "tops: " ++ show ( t ) ++ "\nbottoms:" ++ show ( b ) ++ "\n"


-- Generate numbers of a Model
numbers :: Model -> [Int]
numbers [] = []
numbers m = [ _numberBrick b m | b <- m ]

-- Generate a bitstring representation of an Int
bitString4 :: Int -> String
bitString4 a = intercalate "" [ if testBit a x then "1" else "0" | x <- [3,2,1,0]]

-- Generate a bitstring representation of an Int
bitString :: Int -> String
bitString a = intercalate "" [ if testBit a x then "1" else "0" | x <- [13,12..0]]

-- Generate bitstrings for a Model
class BitString a where
	bitStrings :: [a] -> [String]

-- Bitstrings for a list of Ints
instance BitString Int where
	bitStrings [] = []
	bitStrings m = [ bitString i | i <- m ]

-- Bitstring for a list of Bricks ( a Model)
instance BitString Brick where
	bitStrings [] = []
	bitStrings m = [ bitString i | i <- numbers m ]


-- Vector tuples for a Model
vectors :: Model -> [(Int, Int, Int)]
vectors [] = []
vectors m = [(x l, y l, z l) | l <- _origins m]


-- gen = mkStdGen(1)
-- ms = models gen 100 10 []
-- m = ms !! 98
-- n = numbers m
-- nsb = [ SolverBrick i nr | (i, nr) <- zip [0..] n]
-- p = [_possible a b | (a,b) <- [_nxt i nsb | i <- [0..length nsb - 1]]]
-- bs = bitStrings m

-- big-O challenge
challenge = [122, 2769, 2769, 2809, 3369, 3369, 3449, 4009, 4049, 6016]
sbchallenge = [SolverBrick i nr | (i, nr) <- zip [0..] challenge]
pchallenge = [_possible a b | (a,b) <- [_nxt i sbchallenge | i <- [0..length sbchallenge - 1]]]
bschallenge = bitStrings challenge



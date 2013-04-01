module Brick ( models' ) where

import qualified Data.Set as Set
import System.Random

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
okQuadrant xs = [a | a <- xs, z a > 0 && x a > 0 && y a > 0]

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
topConnectionPoints brick = okQuadrant $ makeConnectionPoints 2 brick
-- Give a list of connection points on the bottom of a brick
bottomConnectionPoints :: Brick -> [Location]
bottomConnectionPoints brick = okQuadrant $ makeConnectionPoints (-2) brick

-- give a list of all connection points for a brick
connectionPoints :: Brick -> [Location]
connectionPoints brick = topConnectionPoints brick ++ bottomConnectionPoints brick

-- Give a list of locations that cannot be occupied
blockLocations :: Brick -> [Location]
blockLocations brick = makeConnectionPoints 0 brick


-- Give the list origins for a list of bricks
origs :: [Brick] -> [Location]
origs [] = []
origs bs = [loc b | b <- bs]

-- Give all the potential locations around given bricks
modelLocations :: [Brick] -> [Location]
modelLocations [] = []
modelLocations (x:xs) = modelLocations xs ++ connectionPoints x

-- Give all the blocking locations of given bricks
modelBlockLocations :: [Brick] -> [Location]
modelBlockLocations [] = []
modelBlockLocations (x:xs) = modelBlockLocations xs ++ blockLocations x

-- Return a random location from a list of locations
pickConnection :: Int -> [Location] -> Location
pickConnection _ [] = Location 1 1 1
pickConnection seed xs = xs !! ( fst $ randomR(0, length xs - 1) (mkStdGen ( seed * 1337 ) ) )

-- Generate a model of n bricks into list x
model' :: (Num i, Ord i) => i -> Int -> [Brick] -> [Brick]
model' n seed x
	| n < 0 = []
	| otherwise = x ++ model' (n-1) seed [brick]
		where
			blockS = Set.fromList $ modelBlockLocations x
			allS = Set.fromList $ modelLocations x
			availableL = Set.toList $ ( Set.difference allS blockS )
			location = pickConnection seed availableL
			brick = Brick $ location

-- Generate c models of n bricks into list m of Models
models' :: Int -> Int -> [Brick] -> [Model]
models' c n m
	| c <= 0 = []
	| otherwise = [model' n c []] ++ models' (c-1) n m


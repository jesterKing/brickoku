module Brick ( models', renderModel, putModel ) where

import Data.Set ( fromList, toList, difference )
import Data.List ( intercalate )
import System.Random ( StdGen, randomR, next )

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
pickConnection :: StdGen -> [Location] -> (Location, StdGen)
pickConnection rgen [] = (Location 1 1 1 , rgen)
pickConnection rgen xs = (xs !! pos , rgen2)
	where
		(pos, rgen2) = randomR (0, length xs-1) rgen

-- Generate a model of n bricks into list x
model' :: (Num i, Ord i) => i -> StdGen -> [Brick] -> [Brick]
model' n rgen x
	| n < 0 = []
	| otherwise = x ++ model' (n-1) rgen2 [brick]
		where
			blockS = fromList $ modelBlockLocations x
			allS = fromList $ modelLocations x
			availableL = toList $ difference allS blockS
			(location, rgen2) = pickConnection rgen availableL
			brick = Brick location

-- Generate c models of n bricks into list m of Models
models' :: StdGen -> Int -> Int -> [Brick] -> [Model]
models' rgen count brickcount mlist
	| count <= 0 = []
	| otherwise = [model' brickcount rgen []] ++ models' rgen2 (count-1) brickcount mlist
		where
			(x, rgen2) = next rgen

renderModel :: Model -> String
renderModel m = "[" ++ bricks m ++ "]"
	where
		bricks [] = ""
		bricks ls = intercalate ", " (map renderBrick ls)
		renderBrick b = "(" ++ ( show $ x $ loc b ) ++ ", " ++ ( show $ y $ loc b ) ++ ", " ++ ( show $ z $ loc b ) ++ ")"

putModel :: Model -> IO ()
putModel m = putStrLn ( renderModel m )

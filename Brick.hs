import qualified Data.Set as Set
import qualified System.Random as R

data Location = Location {x :: Int, y :: Int, z :: Int}
	deriving (Show, Ord, Eq)


data Brick = Brick { loc :: Location }
	deriving (Show, Ord, Eq)

type Model = [Brick]

okQuadrant :: [Location] -> [Location]
okQuadrant [] = error "empty list"
okQuadrant xs = [a | a <- xs, z a > 0]

-- base locations contain X and Y of brick openings
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
createLocations :: Int -> Brick -> [Location]
createLocations n brick = map (\(a, b) -> Location (a+x') (b+y') (n+z')) baseLocations 
	where
		x' = x ( loc brick )
		y' = y ( loc brick )
		z' = z ( loc brick )

topLocations :: Brick -> [Location]
topLocations brick = okQuadrant $ createLocations 2 brick
blockLocations :: Brick -> [Location]
blockLocations brick = okQuadrant $ createLocations 0 brick
bottomLocations :: Brick -> [Location]
bottomLocations brick = okQuadrant $ createLocations (-2) brick

b1 = Brick $ Location 1 1 1
b2 = Brick $ Location 1 1 3
b3 = Brick $ Location 1 2 1

allLocations :: Brick -> [Location]
allLocations brick = topLocations brick ++ bottomLocations brick

b1all = allLocations b1
b2all = allLocations b2
b3all = allLocations b3

b1set = Set.fromList ( b1all ++ b2all ++ b3all )
--b2set = Set.fromList b2all
--b3set = Set.fromList b3all
b1block = Set.fromList ( blockLocations b1 ++ blockLocations b2 ++ blockLocations b3 )
--b2block = Set.fromList $ blockLocations b2
--b3block = Set.fromList $ blockLocations b3

totfree = length $ Set.toList ( Set.difference b1set b1block )

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- Give the origins of a list of bricks
origs :: [Brick] -> [Location]
origs [] = []
origs bs = [loc b | b <- bs]

-- Give all the potential locations around given bricks
modelLocations :: [Brick] -> [Location]
modelLocations [] = []
modelLocations (x:xs) = modelLocations xs ++ allLocations x

-- Give all the blocking locations of given bricks
modelBlockLocations :: [Brick] -> [Location]
modelBlockLocations [] = []
modelBlockLocations (x:xs) = modelBlockLocations xs ++ blockLocations x

-- Return a random location from a list of locations
giveLocation :: Int -> [Location] -> Location
giveLocation _ [] = Location 1 1 1
giveLocation r xs = xs !! ( fst $ R.randomR(0, length xs - 1) (R.mkStdGen r) )

-- Generate a model of n bricks, put in the list given
model' :: (Num i, Ord i) => i -> Int -> [Brick] -> [Brick]
model' n r x
	| n < 0 = []
	| otherwise = x ++ model' (n-1) r [brick]
		where
			blockS = Set.fromList $ modelBlockLocations x
			allS = Set.fromList $ modelLocations x
			availableL = Set.toList $ ( Set.difference allS blockS )
			location = giveLocation r availableL
			brick = Brick $ location

models' :: Int -> Int -> [Brick] -> [Model]
models' c n m
	| c <= 0 = []
	| otherwise = [model' n c []] ++ models' (c-1) n m


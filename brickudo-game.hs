import Brick
import Data.Set as Set
import System.Environment( getArgs )

main = do
	-- get command-line arguments
	args <- getArgs

 -- read model count from arguments, defaulting to 1 if empty
	let [from, to]
		| length args == 2 = [read (args !! 0) :: Int, read (args !! 1) :: Int]
		| otherwise = [0, 1]

	-- create the models
	let models = models' from to 10 []
	let smodels = Set.fromList models
	let unique_models = Set.toList smodels

	-- output the models
	print unique_models
	print $ length models
	print $ length unique_models

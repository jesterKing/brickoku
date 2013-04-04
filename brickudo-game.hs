import Brick
import Data.Set ( fromList, toList )
import Data.List ( intercalate )
import System.Environment ( getArgs )
import System.Random ( newStdGen, mkStdGen, next )

main = do
	-- get command-line arguments
	args <- getArgs

	defgen <- newStdGen

 -- read model count from arguments, defaulting to 1 if empty
	let [seed, count]
		| length args == 2 = [read (args !! 0) :: Int, read (args !! 1) :: Int]
		| length args == 1 = [a, read (args !! 0) :: Int]
		| otherwise = [a, b]
			where
				(a, defgen2) = next defgen
				b = 1

	-- create the models
	let rgen = mkStdGen ( seed )
	let models = models' rgen count 10 []
	let smodels = fromList models
	let unique_models = toList smodels

	-- output the models
	putStrLn ("seed = " ++ show(seed))
	-- sequence [ putModel m | m <- unique_models ]
	let ls = intercalate ",\n" (map renderModel unique_models)
	putStrLn ("ls = [" ++ ls ++ "]")

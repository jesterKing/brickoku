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
	let [seed, brick_count, model_count]
		| length args == 3 = [read (args !! 0) :: Int, read (args !! 1) :: Int, read (args !! 2) :: Int]
		| length args == 2 = [c, read (args !! 0) :: Int, read (args !! 1) :: Int]
		| length args == 1 = [c, b, read (args !! 0) :: Int]
		| otherwise = [c, b, a]
			where
				(c, defgen2) = next defgen
				b = 10
				a = 1

	-- create the models
	let rgen = mkStdGen ( seed )
	let models = models' rgen model_count brick_count []
	let smodels = fromList models
	let unique_models = toList smodels

	-- output the models
	putStrLn ("seed = " ++ show(seed))
	putStrLn ("bricks_per_model = " ++ show(brick_count))
	-- sequence [ putModel m | m <- unique_models ]
	let ls = intercalate ",\n" (map renderModel unique_models)
	putStrLn ("ls = [" ++ ls ++ "]")

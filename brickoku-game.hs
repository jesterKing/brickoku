import Brick
import Data.Set ( fromList, toList )
import Data.List ( intercalate )
import System.Environment ( getArgs )
import System.Random ( newStdGen, mkStdGen, next )

import GHC.Conc
import System.Info
import Text.Printf
import Data.Version

main = do
	printf "Compiled with %s-%s on %s/%s\n"
		compilerName
		(showVersion compilerVersion)
		os arch

	printf "Running with %d OS threads\n" numCapabilities
	-- get command-line arguments
	args <- getArgs

	defgen <- newStdGen

 -- read model count from arguments, defaulting to 1 if empty
	let [seed, brick_count, model_count]
		| length args == 3 = [read (args !! 0) :: Int, read (args !! 1) :: Int,
													read (args !! 2) :: Int]
		| length args == 2 = [c, read (args !! 0) :: Int, read (args !! 1) :: Int]
		| length args == 1 = [c, b, read (args !! 0) :: Int]
		| otherwise = [c, b, a]
			where
				(c, defgen2) = next defgen
				b = 10
				a = 1

	-- create the models
	let rgen = mkStdGen ( seed )
	let ms = models rgen model_count brick_count []
	let ls = ["(\n\t" ++ show (vectors m) ++
						",\n\t" ++ show (numbers m) ++
						",\n\t" ++ show (bitStrings m) ++ "\n)" | m <- ms ]

	-- output the models
	putStrLn ("# VERSION = 0.0.1")
	putStrLn ("seed = " ++ show(seed))
	putStrLn ("bricks_per_model = " ++ show(brick_count))
	putStrLn ("model_count = " ++ show(model_count))
	putStrLn ("ls = [\n" ++ (intercalate ",\n" ls) ++ "]\n")
	putStrLn ("\n")
	putStrLn( "\n\t" ++ show (challenge) ++ "\n")
	putStrLn( "\n\t" ++ show (sbchallenge) ++ "\n")
	putStrLn( "\n\t" ++ show (bschallenge) ++ "\n")
	putStrLn( "\n\t" ++ show (pchallenge) ++ "\n")
	putStrLn( "\n\n" ++ renderPossibles ( pchallenge ) ++ "\n")

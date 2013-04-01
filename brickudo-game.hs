import Brick
import System.Environment( getArgs )

main = do
	-- get command-line arguments
	args <- getArgs

 -- read model count from arguments, defaulting to 1 if empty
	let model_count
		| length args == 0 = 1
		| otherwise = read ( head args ) :: Int

	-- create the models
	let models = models' model_count 10 []

	-- output the models
	print models

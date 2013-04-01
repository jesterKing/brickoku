import Brick
import System.Environment( getArgs )

main = do
	[arg] <- getArgs
	let model_count = read arg :: Int
	let models = models' model_count 10 []
	print models
	print $ length models

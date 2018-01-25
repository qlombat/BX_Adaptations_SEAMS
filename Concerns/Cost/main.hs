import Concerns.Cost.Analysis
import Concerns.Cost.Plan
import Concerns.Cost.Model
import Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Aeson

import System.Environment
main = do
    args <- getArgs
    res <- case args of
        "--cost":cost:[src] -> return $ costPlan (read cost::Double) (unseralizeC src)
        [src] -> return $ costPlan 1.5 (unseralizeC src)
        otherwise -> error "Error"
    print $ Char8.unpack $ encode res

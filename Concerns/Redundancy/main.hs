import Concerns.Redundancy.Plan
import Concerns.Redundancy.Model
import Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Aeson

import System.Environment
main = do
    args <- getArgs
    res <- case args of
        [src] -> return $ redundancyPlan (unseralizeR src)
        otherwise -> error "Error"
    print $ Char8.unpack $ encode res

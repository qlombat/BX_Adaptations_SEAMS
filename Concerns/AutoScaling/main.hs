import Concerns.AutoScaling.Analysis
import Concerns.AutoScaling.Plan
import Concerns.AutoScaling.Model
import Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Aeson

import System.Environment
main = do
    args <- getArgs
    res <- case args of
        "--load":load:[src] -> return $ autoScalingPlan (autoScalingAnalysis (read load::Double) (unseralizeAS src)) (unseralizeAS src)
        [src] -> return $ autoScalingPlan (autoScalingAnalysis 0.4 (unseralizeAS src)) (unseralizeAS src)
        otherwise -> error "Error"
    print $ Char8.unpack $ encode res

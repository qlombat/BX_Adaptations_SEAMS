import Concerns.Firewall.Plan
import Concerns.Firewall.Model
import Data.ByteString.Lazy.Char8 as Char8 (unpack)
import Data.Aeson

import System.Environment
main = do
    args <- getArgs
    res <- case args of
        [src] -> return $ firewallPlan (unseralizeF src)
        otherwise -> error "Error"
    print $ Char8.unpack $ encode res

module Plumbing where

import Servant (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)

withApp :: Application -> (Warp.Port -> IO ()) -> IO ()
withApp app = Warp.testWithApplication (pure app)

runServer :: Application -> Int -> IO ()
runServer app portNr = run portNr (simpleCors app)
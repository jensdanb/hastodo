{-# LANGUAGE OverloadedStrings #-}
module Plumbing where

import Servant (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCors, simpleCorsResourcePolicy, CorsResourcePolicy(..), simpleHeaders, Origin)
import Network.Wai (Middleware)

localAddr :: Origin
localAddr = "http://localhost:"

runServer :: Middleware -> Application -> Int -> IO ()
runServer mWare app portNr = run portNr (mWare app)

runServerSimpleCors :: Application -> Int -> IO ()
runServerSimpleCors = runServer simpleCors

myCorsPolicy :: Network.Wai.Middleware.Cors.CorsResourcePolicy
myCorsPolicy = simpleCorsResourcePolicy { corsOrigins = Just (map (localAddr <>) ["5173", "5050"],  True)
                                        , corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
                                        , corsRequestHeaders = ["Authorization", "Content-Type"] }

myCors :: Middleware
myCors = cors $ const (Just myCorsPolicy)

runServerWithCors :: Application -> Int -> IO ()
runServerWithCors = runServer myCors


withApp :: Application -> (Warp.Port -> IO ()) -> IO ()
withApp app = Warp.testWithApplication (pure app)

{-# LANGUAGE OverloadedStrings #-}
module Plumbing where

import Servant (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCors, simpleCorsResourcePolicy, CorsResourcePolicy(..), simpleHeaders)
import Network.Wai (Middleware)

runServer :: Middleware -> Application -> Int -> IO ()
runServer mWare app portNr = run portNr (mWare app)

runServerSimpleCors :: Application -> Int -> IO ()
runServerSimpleCors = runServer simpleCors

myCorsPolicy :: Network.Wai.Middleware.Cors.CorsResourcePolicy
myCorsPolicy = simpleCorsResourcePolicy   { corsOrigins = Just (["http://localhost:5173"],  True)
                                    , corsMethods = ["OPTIONS", "GET", "PUT", "POST", "DELETE"]
                                    , corsRequestHeaders = ["Authorization", "Content-Type"] }

myCors :: Middleware
myCors = cors $ const (Just myCorsPolicy)

runServerWithCors :: Application -> Int -> IO ()
runServerWithCors = runServer myCors


withApp :: Application -> (Warp.Port -> IO ()) -> IO ()
withApp app = Warp.testWithApplication (pure app)

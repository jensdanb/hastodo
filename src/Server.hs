{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Api

server :: Server API
server = return "connected"

app :: Application
app = serve api server

serveTodo :: IO ()
serveTodo = run 8080 (simpleCors app)
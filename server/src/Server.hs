{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Servant
import Api

server :: Server API
server = do 
  return "connected"

app :: Application
app = serve api server

serveTodo :: Int -> IO ()
serveTodo portNr = run portNr (simpleCors app)
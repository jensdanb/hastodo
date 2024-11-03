{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import MetaApi (metaApi, MetaAPI)

server :: Server MetaAPI
server = do 
  return "connected"

app :: Application
app = serve metaApi server

serveTodo :: Int -> IO ()
serveTodo portNr = run portNr (simpleCors app)
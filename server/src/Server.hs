{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Server where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import MetaApi (MetaAPI)

serveMetaAPI :: Server MetaAPI
serveMetaAPI = pure "connected"

app :: Application
app = serve (Proxy :: Proxy MetaAPI) serveMetaAPI

serveTodo :: Int -> IO ()
serveTodo portNr = run portNr (simpleCors app)
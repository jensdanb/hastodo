{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (api, API) where

import Servant

import Models (ServerConnected)

type API = "serverConnected" :> Get '[JSON] String

api :: Proxy API
api = Proxy


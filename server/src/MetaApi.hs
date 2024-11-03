{-# LANGUAGE DataKinds, TypeOperators #-}

module MetaApi (metaApi, MetaAPI) where

import Servant
import Data.Aeson

import Models (ServerConnected)

type MetaAPI = "serverConnected" :> Get '[JSON] String

metaApi :: Proxy MetaAPI
metaApi = Proxy


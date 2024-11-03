{-# LANGUAGE DataKinds, TypeOperators #-}

module MetaApi (MetaAPI) where

import Servant
import Data.Aeson

import Models (ServerConnected)

type MetaAPI = "serverConnected" :> Get '[JSON] String



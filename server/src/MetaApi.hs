{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MetaApi (MetaAPI) where

import Data.Aeson (ToJSON)
import GHC.Generics ( Generic )
import Servant ( JSON, ReqBody, type (:>), Get, Post )

-- import Models (ServerConnected)

type MetaAPI = "serverConnected" :> Get '[JSON] String


type TodoAPI = "new_todo" :> ReqBody '[JSON] Todo :> Post '[JSON] Todo

data Todo = Todo
    { id :: String
    , name :: String
    , completed :: Bool
    } deriving (Eq, Show, Generic)

instance ToJSON Todo
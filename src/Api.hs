{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Servant

type API = "hello" :> Get '[JSON] String

api :: Proxy API
api = Proxy
{-# LANGUAGE DataKinds, TypeOperators, 
    DeriveGeneric, OverloadedStrings #-}

module TestAPIs where 

import Data.Text 
import GHC.Generics
import Data.Aeson
import Servant
import Network.Wai (Application)

import Server (serveTodo)
import MetaApi (metaApi, MetaAPI)

data User = User {
  name :: Text
  , user_id :: Integer
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

type UserAPI = "user" :> Capture "userId" Integer :> Post '[JSON] User

userApp :: Application
userApp = serve (Proxy :: Proxy UserAPI) userServer

userServer :: Server UserAPI
userServer = createUser

createUser :: Integer -> Handler User
createUser userId = do
  if userId > 5000
    then pure $ User { name = "some user", user_id = userId }
    else throwError $ err400 { errBody = "userId is too small" }

{-# LANGUAGE DataKinds, TypeOperators, 
    DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module E2E (spec) where 

import Data.Text 
import GHC.Generics
import Data.Aeson
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
import Servant.Client (
    client
  , parseBaseUrl
  , mkClientEnv
  , runClientM
  , baseUrlPort
  )
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec (Spec, around, runIO, describe, shouldBe, it)


import MetaApi (metaApi, MetaAPI)

spec :: Spec
spec = do 
  businessLogicSpec
  -- thirdPartyResourceSpec
  -- servantQuickCheckSpec

-- Testsubject API -- 

data User = User {
  name :: Text
  , user_id :: Integer
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

type UserAPI = "user" :> Capture "userId" Integer :> Post '[JSON] User

-- Business logic -- 

createUser :: Integer -> Handler User
createUser userId = do
  if userId > 5000
    then pure $ User { name = "some user", user_id = userId }
    else throwError $ err400 { errBody = "userId is too small" }

-- Testserver -- 

userApp :: Application
userApp = serve (Proxy :: Proxy UserAPI) userServer

userServer :: Server UserAPI
userServer = createUser

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action = 
  Warp.testWithApplication (pure userApp) action

businessLogicSpec :: Spec 
businessLogicSpec = 
  around withUserApp $ do 
    let testCreateUser = client (Proxy :: Proxy UserAPI)

    baseUrl <- runIO $ parseBaseUrl "http:/localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    describe "POST /user" $ do
      it "should create a user with a high enough ID" $ \port -> do
        result <- runClientM (testCreateUser 50001) (clientEnv port)
        result `shouldBe` (Right $ User {name="some user", user_id=50001})
        
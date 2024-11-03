{-# LANGUAGE DataKinds, TypeOperators, 
    DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}

module E2E (spec) where

import Data.Text ( Text )
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON )
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant
    ( Proxy(..),
      serve,
      err400,
      throwError,
      type (:<|>)(..),
      Capture,
      JSON,
      type (:>),
      Post,
      Server,
      Handler,
      ServerError(errBody),
      Application )
import Servant.Client (client, parseBaseUrl,
                      mkClientEnv, runClientM, baseUrlPort)
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec (Spec, around, runIO,
                  describe, shouldBe, it)
import Data.Either (isLeft)

import MetaApi (MetaAPI)

-- Exports: --

spec :: Spec
spec = do
  testSpec
  -- thirdPartyResourceSpec
  -- servantQuickCheckSpec

-- Testsubject API -- 

type TestSubjectAPI = MockAPI :<|> MetaAPI

type MockAPI = "user" :> Capture "userId" Integer :> Post '[JSON] User

data User = User {
  name :: Text
  , user_id :: Integer
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

-- Business logic -- 

createUser :: Integer -> Handler User
createUser userId = do
  if userId > 5000
    then pure $ User { name = "some user", user_id = userId }
    else throwError $ err400 { errBody = "userId is too small" }

checkMeta :: Handler String
checkMeta = pure "connected"

-- Testserver -- 

testServer :: Server TestSubjectAPI
testServer = createUser
        :<|> checkMeta

testApp :: Application
testApp = serve (Proxy :: Proxy TestSubjectAPI) testServer

withTestApp :: (Warp.Port -> IO ()) -> IO ()
withTestApp action = Warp.testWithApplication (pure testApp) action

-- Running the tests --

testSpec :: Spec
testSpec =
  around withTestApp $ do
    let testMockAPI = client (Proxy :: Proxy MockAPI)
    let testMetaAPI = client (Proxy :: Proxy MetaAPI)

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    -- MockAPI
    describe "POST /user" $ do
      it "should create a user with a high enough ID" $ \port -> do
        result <- runClientM (testMockAPI 50001) (clientEnv port)
        result `shouldBe` (Right $ User {name="some user", user_id=50001})
      it "will it fail with a too-small ID?" $ \port -> do
        result <- runClientM (testMockAPI 4999) (clientEnv port)
        isLeft result `shouldBe` True -- Expectation: Left (FailureResponse _)

    -- MetaAPI
    describe "GET /serverConnected" $ do
      it "should return JSON plaintext: connected" $ \port -> do
        result <- runClientM testMetaAPI (clientEnv port)
        result `shouldBe` Right "connected"
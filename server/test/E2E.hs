{-# LANGUAGE DataKinds, TypeOperators, 
    DeriveGeneric, OverloadedStrings #-}

module E2E (spec) where

import Data.Text ( Text )
import GHC.Generics ( Generic )
import Data.Aeson ( FromJSON, ToJSON, encode )
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

import Models
import Api
import Plumbing
import qualified Data.Map as Map

-- Exports: --

spec :: Spec
spec = do
  testSpec
  todoSpec
  -- thirdPartyResourceSpec
  -- servantQuickCheckSpec

--- October API --- 

type OctoberAPI = EPmockUser :<|> EPmeta

type EPmockUser = "mockuser" :> Capture "mockUserId" Integer :> Post '[JSON] MockUser

data MockUser = MockUser {
  mockName :: Text
  , mockUser_id :: Integer
  } deriving (Eq, Show, Generic)

instance FromJSON MockUser
instance ToJSON MockUser


postMockHandler :: Integer -> Handler MockUser
postMockHandler mockUserId = do
  if mockUserId > 5000
    then pure $ MockUser { mockName = "some user", mockUser_id = mockUserId }
    else throwError $ err400 { errBody = "mockUserId is too small" }


serveOctoberAPI :: Server OctoberAPI
serveOctoberAPI = postMockHandler
        :<|> metaEPHandler

--- Mock Server --- 

mockApp :: Application
mockApp = serve (Proxy :: Proxy OctoberAPI) serveOctoberAPI

withMockApp :: (Warp.Port -> IO ()) -> IO ()
withMockApp = withApp mockApp

--- Running the tests ---

testSpec :: Spec
testSpec =
  around withMockApp $ do
    let testMockAPI = client (Proxy :: Proxy EPmockUser)
    let testEPmeta = client (Proxy :: Proxy EPmeta)

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    -- OctoberAPI
    describe "GET /serverConnected :<|> POST /user" $ do
      it "should return JSON plaintext: connected" $ \port -> do
        result <- runClientM testEPmeta (clientEnv port)
        result `shouldBe` Right "connected"
      it "should create a user with a high enough ID" $ \port -> do
        result <- runClientM (testMockAPI 50001) (clientEnv port)
        result `shouldBe` (Right $ MockUser {mockName="some user", mockUser_id=50001})
      it "will it fail with a too-small ID?" $ \port -> do
        result <- runClientM (testMockAPI 4999) (clientEnv port)
        isLeft result `shouldBe` True -- Expectation: Left (FailureResponse _)

--- Todo API ---

testApp :: TodoMap -> Application
testApp todoMap = serve (Proxy :: Proxy TodoAPI) (serveTodoAPI todoMap)

withTestApp :: TodoMap -> (Warp.Port -> IO ()) -> IO ()
withTestApp todoMap = withApp $ testApp todoMap

todoSpec :: Spec
todoSpec =
  around (withTestApp Map.empty) $ do
    let postTodoTester = client (Proxy :: Proxy EPpostTodo)
    let getTodosTester = client (Proxy :: Proxy EPgetTodos)

    let mockUUID = "sgsgerjkg"
    let mockTodo = Todo {uuid=mockUUID, name="Eat", completed=False}

    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl {baseUrlPort = port})

    describe "GET /getTodos >> POST /postTodo >> GET /getTodos" $ do
      it "should return an empty JSON" $ \port -> do
        result <- runClientM getTodosTester (clientEnv port)
        result `shouldBe` Right Map.empty
      it "should insert a Todo" $ \port -> do
        result <- runClientM (postTodoTester mockTodo) (clientEnv port)
        result `shouldBe` Right (Map.fromList [(mockUUID, mockTodo)])

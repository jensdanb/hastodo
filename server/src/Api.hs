{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Models (TodoMap, State(State, todos), initialize, insertTodo, insertMocks, TodoList, Todo)
import Plumbing (runServer)
import Control.Concurrent.STM (atomically, readTVar, writeTVar, readTVarIO)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Monad.Reader (liftIO)
import qualified Data.Map as Map

---
--- Server 
--- 

type AppM = ReaderT State Handler

stmApp :: State -> Application
stmApp state = serve stmAPI $ hoistServer stmAPI (nt state) serveSTM

nt :: State -> AppM a -> Handler a
nt state x = runReaderT x state

runStmServer :: Int -> IO ()
runStmServer port = do
    startState <- initialize
    runServer (stmApp (State startState)) port


runStmServerWithMocks :: Int -> IO ()
runStmServerWithMocks port = do
    startState <- initialize
    liftIO $ insertMocks startState
    runServer (stmApp (State startState)) port

---
--- API toplevel 
--- 

type STMAPI = EPmeta
        :<|> STMpost
        :<|> STMget

serveSTM :: ServerT STMAPI AppM
serveSTM = metaEPHandler
        :<|> stmPost
        :<|> stmGet

stmAPI :: Proxy STMAPI
stmAPI = Proxy

--- 
--- API endpoints
--- 

type EPmeta = "serverConnected" :> Get '[JSON] String

metaEPHandler :: AppM String
metaEPHandler = return "connected"

type STMpost = "stmPost" :> ReqBody '[JSON] Todo :> PostCreated '[JSON] Todo

stmPost :: Todo -> AppM Todo
stmPost newTodo = do
    State{todos = todoVar} <- ask
    liftIO $ insertTodo todoVar newTodo
    return newTodo

type STMget = "stmGet" :> Get '[JSON] TodoList

stmGet :: AppM TodoList
stmGet = do
    State{todos = todoVar} <- ask
    liftIO $ reverse <$> readTVarIO todoVar
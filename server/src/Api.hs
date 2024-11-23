{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Models (State(State, todos), initialize, insertTodo, insertMocks, TodoList, Todo(..), deleteTodo, putTodo, UUID, PutData)
import Plumbing (runServerWithCors)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Monad.Reader (liftIO)

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
    runServerWithCors (stmApp (State startState)) port


runStmServerWithMocks :: Int -> IO ()
runStmServerWithMocks port = do
    startState <- initialize
    liftIO $ insertMocks startState
    runServerWithCors (stmApp (State startState)) port

---
--- API toplevel 
--- 

type STMAPI = EPmeta
        :<|> STMpost
        :<|> STMget
        :<|> STMdelete
        :<|> STMput

serveSTM :: ServerT STMAPI AppM
serveSTM = metaEPHandler
        :<|> stmPost
        :<|> stmGet
        :<|> stmDelete
        :<|> stmPut

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
    liftIO $ insertTodo newTodo todoVar
    return newTodo

type STMget = "stmGet" :> Get '[JSON] TodoList

stmGet :: AppM TodoList
stmGet = do
    State{todos = todoVar} <- ask
    liftIO $ reverse <$> readTVarIO todoVar

type STMdelete = "stmDelete" :> ReqBody '[JSON] UUID :> Delete '[JSON] UUID

stmDelete :: UUID -> AppM UUID
stmDelete uuid = do
    State{todos = todoVar} <- ask
    liftIO $ deleteTodo uuid todoVar
    return uuid

type STMput = "stmPut" :> ReqBody '[JSON] PutData :> Put '[JSON] PutData

stmPut :: PutData -> AppM PutData
stmPut putData = do
    State{todos = todoVar} <- ask
    liftIO $ putTodo putData todoVar
    return putData
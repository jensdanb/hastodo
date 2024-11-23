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
        :<|> PostTodo
        :<|> GetTodos
        :<|> DelTodo
        :<|> PutTodo

serveSTM :: ServerT STMAPI AppM
serveSTM = handleStatusMessage
        :<|> handlePostTodo
        :<|> handleGetTodos
        :<|> handleDelTodo
        :<|> handlePutTodo

stmAPI :: Proxy STMAPI
stmAPI = Proxy

--- 
--- API endpoints
--- 

type EPmeta = "serverConnected" :> Get '[JSON] String

handleStatusMessage :: AppM String
handleStatusMessage = return "connected"

type PostTodo = "postTodo" :> ReqBody '[JSON] Todo :> PostCreated '[JSON] Todo

handlePostTodo :: Todo -> AppM Todo
handlePostTodo newTodo = do
    State{todos = todoVar} <- ask
    liftIO $ insertTodo newTodo todoVar
    return newTodo

type GetTodos = "getTodos" :> Get '[JSON] TodoList

handleGetTodos :: AppM TodoList
handleGetTodos = do
    State{todos = todoVar} <- ask
    liftIO $ reverse <$> readTVarIO todoVar

type DelTodo = "delTodo" :> ReqBody '[JSON] UUID :> Delete '[JSON] UUID

handleDelTodo :: UUID -> AppM UUID
handleDelTodo uuid = do
    State{todos = todoVar} <- ask
    liftIO $ deleteTodo uuid todoVar
    return uuid

type PutTodo = "putTodo" :> ReqBody '[JSON] PutData :> Put '[JSON] PutData

handlePutTodo :: PutData -> AppM PutData
handlePutTodo putData = do
    State{todos = todoVar} <- ask
    liftIO $ putTodo putData todoVar
    return putData
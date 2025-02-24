{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Models (State(State, todos), TodoList, Todo(..), UUID, 
    initialize, insertTodo, insertMocks, deleteTodo, putTodo, insertTodos, todoExists, overlap')
import Plumbing (runServerWithCors)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Monad.Reader (liftIO)
import Data.Text.Lazy.Encoding (encodeUtf8)

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
        :<|> PostTodos
        :<|> GetTodos
        :<|> DelTodo
        :<|> PutTodo

serveSTM :: ServerT STMAPI AppM
serveSTM = handleStatusMessage
        :<|> handlePostTodo
        :<|> handlePostTodos
        :<|> handleGetTodos
        :<|> handleDelTodo
        :<|> handlePutTodo

stmAPI :: Proxy STMAPI
stmAPI = Proxy

--- 
--- API endpoints
--- 

type EPmeta = "serverConnected" :> Get '[JSON] Bool

handleStatusMessage :: AppM Bool
handleStatusMessage = return True

type PostTodo = "postTodo" :> ReqBody '[JSON] Todo :> PostCreated '[JSON] Todo

handlePostTodo :: Todo -> AppM Todo
handlePostTodo newTodo = do
    State{todos = todoVar} <- ask
    postAllowed <- liftIO $ not <$> todoExists todoVar newTodo
    if postAllowed
        then do 
            liftIO $ insertTodo newTodo todoVar
            return newTodo
        else 
            throwError $ error400idIsUsed newTodo

type PostTodos = "postTodos" :> ReqBody '[JSON] [Todo] :> PostCreated '[JSON] [Todo]

handlePostTodos :: [Todo] -> AppM [Todo]
handlePostTodos newTodos = do
    State{todos = todoVar} <- ask
    postAllowed <- liftIO $ not <$> overlap' todoVar newTodos
    if postAllowed
        then do 
            liftIO $ insertTodos newTodos todoVar
            return newTodos
        else 
            throwError error400idCollision

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

type PutTodo = "putTodo" :> ReqBody '[JSON] Todo :> Put '[JSON] Todo

handlePutTodo :: Todo -> AppM Todo
handlePutTodo newTodo = do
    State{todos = todoVar} <- ask
    liftIO $ putTodo newTodo todoVar
    return newTodo

error400idIsUsed :: Todo -> ServerError
error400idIsUsed newTodo = err400 { errBody = "Todo with ID " <> (encodeUtf8 newTodo.id) <> "already exists" }

error400idCollision :: ServerError
error400idCollision = err400 {errBody = "One of the IDs collided!"}

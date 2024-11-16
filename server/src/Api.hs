{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Data.Text (Text)
import Network.Wai ( Application )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Models (Todo(..), TodoMap, insertTodo, initialState)
import Plumbing (runServer)

--- EPmeta --- 

type EPmeta = "serverConnected" :> Get '[JSON] String

metaEPHandler :: Server EPmeta
metaEPHandler = return "connected"

app :: Application
app = serve (Proxy :: Proxy EPmeta) metaEPHandler

runMetaServer :: Int -> IO ()
runMetaServer = runServer app

--- TodoAPI --- 

type TodoAPI = EPpostTodo
          :<|> EPgetTodos


serveTodoAPI :: TodoMap -> Server TodoAPI
serveTodoAPI todoMap = postTodo todoMap
                    :<|> getTodos todoMap

todoApp :: Application
todoApp = serve (Proxy :: Proxy TodoAPI) (serveTodoAPI initialState)

runTodoServer :: Int -> IO ()
runTodoServer = runServer todoApp

--- 

type EPpostTodo = "postTodo" :> ReqBody '[JSON] Todo :> Post '[JSON] TodoMap

postTodo :: TodoMap -> Todo -> Handler TodoMap
postTodo todoMap newTodo = return $ insertTodo todoMap newTodo

type EPgetTodos = "getTodos" :> "list-all" :> Get '[JSON] TodoMap

getTodos :: TodoMap -> Handler TodoMap
getTodos = return

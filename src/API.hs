{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module API where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server

import           Database (fetchUserPG, createUserPG, fetchPostgresConnection)
import           Schema

type TodosAPI = 
       "todos" :> Capture "todoid" Int64 :> Get '[JSON] Todo
  :<|> "todos" :> ReqBody '[JSON] Todo :> Todo '[JSON] Int64

todoAPI :: Proxy TodosAPI
todosAPI = Proxy :: Proxy TodosAPI

fetchTodosHandler :: ConnectionString -> Int64 -> Handler Todo
fetchTodosHandler connString uid = do
  maybeTodo <- liftIO $ fetchTodoPG connString uid
  case maybeTodo of
    Just todo -> return todo
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find todo with that ID" })

createTodoHandler :: ConnectionString -> Todo -> Handler Int64
createTodoHandler connString todo = liftIO $ createTodoPG connString todo

todosServer :: ConnectionString -> Server TodosAPI
todosServer connString = 
  (fetchTodosHandler connString) :<|> 
  (createTodoHandler connString)


runServer :: IO ()
runServer = do
  connString <- fetchPostgresConnection
  run 8000 (serve todosAPI (todosServer connString))
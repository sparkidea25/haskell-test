{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger (runStdoutLoggingT, MonadLogger, LoggingT)
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist (get, insert, delete)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlConn, runMigration, SqlPersistT)

import           Schema

localConnString :: ConnectionString
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=postgres"

-- This is IO since in a real application we'd want to configure it.
fetchPostgresConnection :: IO ConnectionString
fetchPostgresConnection = return localConnString

runAction :: ConnectionString -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ withPostgresqlConn connectionString $ \backend ->
    runReaderT action backend

migrateDB :: ConnectionString -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

fetchTodoPG :: ConnectionString -> Int64 -> IO (Maybe Todo)
fetchTodoPG connString uid = runAction connString (get (toSqlKey uid))

createTodoPG :: ConnectionString -> Todo -> IO Int64
createTodoPG connString todo = fromSqlKey <$> runAction connString (insert todo)

deleteTodoPG :: ConnectionString -> Int64 -> IO ()
deleteTodoPG connString uid = runAction connString (delete todoKey)
  where
    todoKey :: Key Todo
    todoKey = toSqlKey uid

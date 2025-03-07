{-# LANGUAGE OverloadedStrings #-}

module Database (runDB, withDatabasePool) where

import Database.Persist.Postgresql
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend, runSqlPool, ConnectionPool, createSqlPool)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text, pack)
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (getEnv)

loadEnv :: IO ()
loadEnv = loadFile defaultConfig

connStr :: IO ConnectionString
connStr = do
  host <- pack <$> getEnv "DB_HOST"
  port <- pack <$> getEnv "DB_PORT"
  user <- pack <$> getEnv "DB_USER"
  password <- pack <$> getEnv "DB_PASSWORD"
  name <- pack <$> getEnv "DB_NAME"
  return $ encodeUtf8 $ "host=" <> host <> " dbname=" <> name <> " user=" <> user <> " password=" <> password <> " port=" <> port

-- Run a database action
runDB :: MonadIO m => ReaderT SqlBackend IO a -> ConnectionPool -> m a
runDB query pool = liftIO $ runSqlPool query pool

-- Create a connection pool
withDatabasePool :: (ConnectionPool -> IO ()) -> IO ()
withDatabasePool action = do
    loadEnv
    connectionString <- connStr
    runStdoutLoggingT $ withPostgresqlPool connectionString 10 (liftIO . action)

{-# LANGUAGE OverloadedStrings #-}

module Database (runDB, withDatabasePool) where

import Database.Persist.Postgresql
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend, runSqlPool, ConnectionPool, createSqlPool)
import Data.Text.Encoding (encodeUtf8)
import Config.AppConfig (connStr, loadEnv)

-- Run a database action
runDB :: MonadIO m => ReaderT SqlBackend IO a -> ConnectionPool -> m a
runDB query pool = liftIO $ runSqlPool query pool

-- Create a connection pool
withDatabasePool :: (ConnectionPool -> IO ()) -> IO ()
withDatabasePool action = do
    loadEnv
    connectionString <- connStr
    runStdoutLoggingT $ withPostgresqlPool connectionString 10 (liftIO . action)

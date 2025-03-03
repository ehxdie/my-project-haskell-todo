{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Database (runDB, withDatabasePool) where

import Database.Persist.Postgresql
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.Trans.Reader (ReaderT)
import Database.Persist.Sql (SqlBackend, runSqlPool, ConnectionPool, createSqlPool)

-- Connection string (modify with your database credentials)
connStr :: ConnectionString
connStr = "host=localhost dbname=todo user=postgres password=example port=5432"

-- Run a database action
runDB :: MonadIO m => ReaderT SqlBackend IO a -> ConnectionPool -> m a
runDB query pool = liftIO $ runSqlPool query pool

-- Create a connection pool
withDatabasePool :: (ConnectionPool -> IO ()) -> IO ()
withDatabasePool action =
    runStdoutLoggingT $ withPostgresqlPool connStr 10 action

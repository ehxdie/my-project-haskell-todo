{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server (app, runServer) where

import Servant
import Api
import Handlers
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Database
import Database.Persist.Sql (ConnectionPool, runMigration)
import Models (migrateAll)  

server :: ConnectionPool -> Server API
server pool = getUsers pool
         :<|> createUser pool
         :<|> getTodos pool 
         :<|> postTodo pool 
         :<|> updateTodo pool 
         :<|> deleteTodo pool
         :<|> toggleTodo pool
         :<|> getTodosPage pool
         :<|> getAuthPage pool

app :: ConnectionPool -> Application
app pool = serve api (server pool)

runServer :: IO ()
runServer = withDatabasePool $ \pool -> do
    putStrLn "Running database migrations..."
    runDB (runMigration migrateAll) pool  -- ✅ Apply migrations
    putStrLn "Migrations complete. Starting server..."
    run 8080 (app pool)

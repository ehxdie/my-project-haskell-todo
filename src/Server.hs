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

-- Server implementation split by API type
authServer :: ConnectionPool -> Server AuthAPI
authServer pool = getAuthPage pool
    :<|> getLoginForm
    :<|> getSignupForm
    :<|> loginUser pool

todoServer :: ConnectionPool -> Server TodoAPI
todoServer pool = 
    (\authHeader -> getTodos authHeader pool)      -- Handles AuthHeader first
    :<|> (\authHeader todo -> postTodo authHeader pool todo)
    :<|> (\authHeader todoId todo -> updateTodo authHeader pool todoId todo)
    :<|> (\authHeader todoId -> deleteTodo authHeader pool todoId)
    :<|> (\authHeader todoId -> toggleTodo authHeader pool todoId)
    :<|> (\authHeader -> getTodosPage authHeader pool)
    
userServer :: ConnectionPool -> Server UserAPI
userServer pool = getUsers pool
    :<|> createUser pool

-- Combined server
server :: ConnectionPool -> Server API
server pool = authServer pool
    :<|> todoServer pool
    :<|> userServer pool

app :: ConnectionPool -> Application
app pool = serve api (server pool)

runServer :: IO ()
runServer = withDatabasePool $ \pool -> do
    putStrLn "Running database migrations..."
    runDB (runMigration migrateAll) pool  -- ✅ Apply migrations
    putStrLn "Migrations complete. Starting server..."
    run 8080 (app pool)

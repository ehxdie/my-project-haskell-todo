{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Handlers.Todo
    ( getTodos
    , getTodosPage
    , postTodo
    , updateTodo
    , deleteTodo
    , toggleTodo
    ) where

import Servant
import Models (Todo(..), todoUserId, User(..), userEmail, EntityField(..), TodoForm(..), todoFromForm)
import Database.Persist.Sql (Entity(..), ConnectionPool, insertEntity, selectList, replace, delete, get, Key, (==.))
import qualified Database.Persist as P
import Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Pages.TodoPage (renderTodosPage, renderTodo)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)
import Control.Monad.Logger (runStdoutLoggingT, logInfo, logDebug)
import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import Config.JwtAuth (requireAuth, AuthenticatedUser(..), AuthHeader)
import Config.AppConfig (getJwtSecret)

-- Helper Function, Getting authenticated user
getAuthenticatedUser :: Maybe T.Text -> ConnectionPool -> Handler (Entity User)
getAuthenticatedUser authHeader pool = do
    secret <- liftIO getJwtSecret
    authenticatedUser <- requireAuth secret authHeader

    -- Fetch the user entity based on the email from the JWT, converting Text to String
    runDB (P.selectFirst [UserEmail ==. T.unpack (Config.JwtAuth.userEmail authenticatedUser)] []) pool >>= \case
        Nothing -> throwError err401 { errBody = "User not found" }
        Just entity -> return entity

-- DATABASE HANDLERS
-- GET /todos (JSON response)
getTodos :: Maybe T.Text -> ConnectionPool -> Handler [Entity Todo]
getTodos authHeader pool = do
    authenticatedUserEntity <- getAuthenticatedUser authHeader pool
    
    -- Get the actual UserId
    let userId = entityKey authenticatedUserEntity

    -- Getting all todos related to the authenticated user
    runDB (selectList [TodoUserId P.==. userId] []) pool

-- POST /todos (HTML response)
postTodo :: Maybe T.Text -> ConnectionPool -> TodoForm -> Handler (Html ())
postTodo authHeader pool todoForm = do

    liftIO $ runStdoutLoggingT $ do
        $(logInfo) $ T.pack "Received POST /todos request"
        $(logDebug) $ T.pack "Headers: " <> T.pack (show authHeader)
        $(logDebug) $ T.pack "Todo data: " <> T.pack (show todoForm)

    authenticatedUserEntity <- getAuthenticatedUser authHeader pool
    
    -- Get the actual UserId
    let userId = entityKey authenticatedUserEntity
        -- Converting the todoFrom type to the todo type, to allow for insertion into the database
        todo = todoFromForm userId todoForm
    
    liftIO $ runStdoutLoggingT $ do
        $(logInfo) $ T.pack "Received POST /todos request"
        let todoData = T.pack (BL.unpack (encode todo))
        $(logDebug) $ T.pack "Received todo data: " <> todoData

    result <- runDB (insertEntity todo) pool
    return $ renderTodo result

-- PUT /todos/:id (JSON response)
updateTodo :: Maybe T.Text -> ConnectionPool -> Key Todo -> TodoForm -> Handler (Html ())
updateTodo authHeader pool todoId todoForm = do

    liftIO $ runStdoutLoggingT $ do
        $(logInfo) $ T.pack "Received PUT /todos request"
        $(logDebug) $ T.pack "Todo data: " <> T.pack (show todoForm)

    authenticatedUserEntity <- getAuthenticatedUser authHeader pool
    
    -- Get the actual UserId
    let userId = entityKey authenticatedUserEntity
        newTodo = todoFromForm userId todoForm

    -- Check if the todo exists
    maybeTodo <- runDB (get todoId) pool
    case maybeTodo of
        Nothing -> throwError err404
        Just _ -> do
            runDB (replace todoId newTodo) pool
            return $ renderTodo (Entity todoId newTodo)

-- DELETE /todos/:id (JSON response)
deleteTodo :: Maybe T.Text -> ConnectionPool -> Key Todo -> Handler NoContent
deleteTodo authHeader pool todoId = do
    secret <- liftIO getJwtSecret
    _ <- requireAuth secret authHeader
    runDB (delete todoId) pool
    return NoContent

-- Toggle todo completion status
toggleTodo :: Maybe T.Text -> ConnectionPool -> Key Todo -> Handler (Html ())
toggleTodo authHeader pool todoId = do

    authenticatedUserEntity <- getAuthenticatedUser authHeader pool
    
    -- Get the actual UserId
    let userId = entityKey authenticatedUserEntity

    maybeTodo <- runDB (get todoId) pool
    case maybeTodo of
        Nothing -> throwError err404
        Just todo -> do
            let updatedTodo = todo { todoCompleted = not (todoCompleted todo), todoUserId = userId }
            runDB (replace todoId updatedTodo) pool
            return $ renderTodo (Entity todoId updatedTodo)

-- HTML Page Handlers
getTodosPage :: Maybe T.Text -> ConnectionPool -> Handler (Html ())
getTodosPage authHeader pool = do
    authenticatedUserEntity <- getAuthenticatedUser authHeader pool
    
    -- Get the actual UserId
    let userId = entityKey authenticatedUserEntity

    todos <- runDB (selectList [TodoUserId P.==. userId] []) pool
    return $ renderTodosPage todos
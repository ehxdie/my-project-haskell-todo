{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers (
    getTodos, postTodo, updateTodo, deleteTodo,
    getTodosPage, getAuthPage, toggleTodo,
    getUsers, createUser,
    loginUser, getLoginForm, getSignupForm  -- Add new exports
) where

import Servant
import Models (Todo(..), User(..), userEmail, userPasswordHash, EntityField(..))
import Database.Persist.Sql (Entity(..), ConnectionPool, insertEntity, selectList, replace, delete, get)
import Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (Key)
import Pages.TodoPage (renderTodosPage, renderTodo)
import Pages.AuthPage (renderAuthPage)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)
import Control.Monad.Logger (runStdoutLoggingT, logInfo, logDebug)
import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Database.Persist as P
import Data.Text (Text)
import Pages.AuthPage (renderLoginForm, renderSignupForm)
import Control.Monad (void, when)

-- USER HANDLERS
getUsers :: ConnectionPool -> Handler [Entity User]
getUsers pool = runDB (selectList [] []) pool

createUser :: ConnectionPool -> User -> Handler (Entity User)
createUser pool user = do
    liftIO $ runStdoutLoggingT $ do
        $(logInfo) $ T.pack "Received signup request"
        $(logDebug) $ T.pack "User email: " <> T.pack (userEmail user)
        
    -- Validate email and password
    when (T.null $ T.pack $ userEmail user) $ 
        throwError err400 { errBody = BL.pack "Email cannot be empty" }
    when (T.null $ T.pack $ userPasswordHash user) $ 
        throwError err400 { errBody = BL.pack "Password cannot be empty" }
        
    -- Check if user already exists
    existing <- runDB (P.selectList [UserEmail P.==. userEmail user] []) pool
    case existing of
        (_:_) -> throwError err409 { errBody = BL.pack "User already exists" }
        [] -> do
            result <- runDB (insertEntity user) pool
            return result

-- Login handler
loginUser :: ConnectionPool -> User -> Handler (Html ())
loginUser pool user = do
    users <- runDB (P.selectList 
        [ UserEmail P.==. userEmail user
        , UserPasswordHash P.==. userPasswordHash user 
        ] []) pool
    case users of
        (user:_) -> do
            -- On success, return the todos page
            todos <- getTodos pool
            return $ renderTodosPage todos
        [] -> throwError err401 { errBody = BL.pack "Invalid credentials" }

-- Get login form handler
getLoginForm :: Handler (Html ())
getLoginForm = return renderLoginForm

-- Get signup form handler
getSignupForm :: Handler (Html ())
getSignupForm = return renderSignupForm

-- DATABASE HANDLERS
-- GET /todos (JSON response)
getTodos :: ConnectionPool -> Handler [Entity Todo]
getTodos pool = runDB (selectList [] []) pool

-- POST /todos (HTML response)
postTodo :: ConnectionPool -> Todo -> Handler (Html ())
postTodo pool todo = do
    liftIO $ runStdoutLoggingT $ do
        $(logInfo) $ T.pack "Received POST /todos request"
        let todoData = T.pack (BL.unpack (encode todo))
        $(logDebug) $ T.pack "Received todo data: " <> todoData
    result <- runDB (insertEntity todo) pool
    return $ renderTodo result

-- PUT /todos/:id (JSON response)
updateTodo :: ConnectionPool -> Key Todo -> Todo -> Handler (Entity Todo)
updateTodo pool todoId newTodo = do
    runDB (replace todoId newTodo) pool
    return (Entity todoId newTodo)

-- DELETE /todos/:id (JSON response)
deleteTodo :: ConnectionPool -> Key Todo -> Handler NoContent
deleteTodo pool todoId = do
    runDB (delete todoId) pool
    return NoContent

-- Toggle todo completion status
toggleTodo :: ConnectionPool -> Key Todo -> Handler (Html ())
toggleTodo pool todoId = do
    maybeTodo <- runDB (get todoId) pool
    case maybeTodo of
        Nothing -> throwError err404
        Just todo -> do
            let updatedTodo = todo { todoCompleted = not (todoCompleted todo) }
            runDB (replace todoId updatedTodo) pool
            return $ renderTodo (Entity todoId updatedTodo)

-- HTML Page Handlers
getTodosPage :: ConnectionPool -> Handler (Html ())
getTodosPage pool = do
    todos <- getTodos pool
    return $ renderTodosPage todos

getAuthPage :: ConnectionPool -> Handler (Html ())
getAuthPage _ = return $ renderAuthPage

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Handlers (getTodos, postTodo, updateTodo, deleteTodo, getTodosPage, getAuthPage) where

import Servant
import Models (Todo(..))
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

-- HTML Page Handlers
getTodosPage :: ConnectionPool -> Handler (Html ())
getTodosPage pool = do
    todos <- getTodos pool
    return $ renderTodosPage todos

getAuthPage :: ConnectionPool -> Handler (Html ())
getAuthPage _ = return $ renderAuthPage

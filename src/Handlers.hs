{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handlers (getTodos, postTodo, updateTodo, deleteTodo, getTodosPage, getAuthPage) where

import Servant
import Models(Todo(..))
import Database.Persist.Sql (Entity(..), ConnectionPool, insertEntity, insert, selectList, (==.), update, replace, delete)
import Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (Key)
import Pages.TodoPage (renderTodosPage)
import Pages.AuthPage (renderAuthPage)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)

-- GET /todos
getTodos :: ConnectionPool -> Handler [Entity Todo]
getTodos pool = runDB (selectList [] []) pool

-- POST /todos
postTodo :: ConnectionPool -> Todo -> Handler (Entity Todo)
postTodo pool todo = runDB (insertEntity todo) pool

-- PUT /todos/:id
updateTodo :: ConnectionPool -> Key Todo -> Todo -> Handler (Entity Todo)
updateTodo pool todoId newTodo = do
    runDB (replace todoId newTodo) pool
    return (Entity todoId newTodo)

-- DELETE /todos/:id
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
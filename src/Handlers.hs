{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Handlers (getTodos, postTodo, updateTodo, deleteTodo) where

import Servant
import Models(Todo(..))
import Database.Persist.Sql (Entity(..), insert, selectList, (==.), update, replace, delete)
import Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (Key)

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
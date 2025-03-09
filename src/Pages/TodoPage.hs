{-# LANGUAGE OverloadedStrings #-}

module Pages.TodoPage (renderTodosPage, renderTodo) where

import Lucid
import Database.Persist.Sql (Entity(..))
import Models (Todo(..))
import Pages.Layout (baseLayout)
import Helpers.Htmx

import qualified Data.Text as T
import Web.HttpApiData (toUrlPiece)

-- Render the full Todos Page using baseLayout
renderTodosPage :: [Entity Todo] -> Html ()
renderTodosPage todos = baseLayout $ do
    div_ [class_ "max-w-lg mx-auto bg-white p-6 rounded shadow-md"] $ do
        h2_ [class_ "text-2xl font-bold mb-4"] "Todo List"

        -- Todo List (HTMX Updates Here)
        div_ [id_ "todo-list"] $
            ul_ $ mapM_ renderTodo todos

        -- Form to Add New Todos (HTMX)
        form_
            [ hxPost_ "/todos"
            , hxTarget_ "#todo-list"
            , hxSwap_ "beforeend"
            , class_ "mt-4 flex flex-col space-y-2"
            ] $ do
            
            -- Todo Title Input
            input_
                [ type_ "text"
                , name_ "todo"
                , placeholder_ "Todo Title"
                , class_ "border p-2 w-full"
                , required_ "true"
                ]

            -- Description Input
            input_
                [ type_ "text"
                , name_ "description"
                , placeholder_ "Description"
                , class_ "border p-2 w-full"
                , required_ "true"
                ]

            -- Completed Checkbox
            div_ [class_ "flex items-center"] $ do
                input_ [type_ "checkbox", name_ "completed", class_ "mr-2"]
                label_ "Completed"

            -- Submit Button
            button_
                [ type_ "submit"
                , class_ "bg-blue-500 text-white px-4 py-2 rounded w-full"
                ] "Add"

-- Render a Single Todo Item as an HTML list item
renderTodo :: Entity Todo -> Html ()
renderTodo (Entity todoId (Todo todo desc completed)) =
    li_ [class_ "p-2 border-b flex justify-between items-center"] $ do
        div_ $ do
            span_ [class_ "font-bold"] $ toHtml todo
            p_ [class_ "text-sm text-gray-600"] $ toHtml desc
            p_ [class_ ("text-sm " <> if completed then "text-green-600" else "text-red-600")] $
                if completed then "Completed ✅" else "Pending ❌"

        button_
            [ hxDelete_ ("/todos/" <> toUrlPiece todoId)
            , hxTarget_ "closest li"
            , hxSwap_ "outerHTML"
            , class_ "bg-red-500 text-white px-2 py-1"
            ] "Delete"

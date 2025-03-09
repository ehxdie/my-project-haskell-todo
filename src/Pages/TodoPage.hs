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
    div_ [class_ "max-w-4xl mx-auto bg-white p-6 rounded shadow-md"] $ do
        h2_ [class_ "text-3xl font-bold mb-6 text-center"] "Todo List"

        -- Main container with flexbox
        div_ [class_ "flex gap-8"] $ do
            -- Form to Add New Todos (Left Side)
            div_ [class_ "w-1/3 bg-gray-100 p-4 rounded-lg"] $ do
                h3_ [class_ "text-lg font-semibold mb-3"] "Add New Todo"
                form_
                    [ hxPost_ "/todos"
                    , hxTarget_ "#todo-list"
                    , hxSwap_ "beforeend"
                    , class_ "flex flex-col space-y-3"
                    ] $ do
                    
                    -- Todo Title Input
                    label_ [class_ "text-sm font-semibold"] "Title"
                    input_
                        [ type_ "text"
                        , name_ "todo"
                        , placeholder_ "Enter title"
                        , class_ "border p-2 w-full rounded"
                        , required_ "true"
                        ]

                    -- Description Input
                    label_ [class_ "text-sm font-semibold"] "Description"
                    input_
                        [ type_ "text"
                        , name_ "description"
                        , placeholder_ "Enter description"
                        , class_ "border p-2 w-full rounded"
                        , required_ "true"
                        ]

                    -- Submit Button
                    button_
                        [ type_ "submit"
                        , class_ "bg-blue-500 hover:bg-blue-600 text-white px-4 py-2 rounded w-full mt-3"
                        ] "Add Todo"

            -- Todo List (Right Side)
            div_ [class_ "w-2/3 bg-gray-50 p-4 rounded-lg"] $ do
                h3_ [class_ "text-lg font-semibold mb-3"] "Your Todos"
                div_ [id_ "todo-list", class_ "space-y-4"] $
                    ul_ [class_ "divide-y divide-gray-300"] $ mapM_ renderTodo todos

-- Render a Single Todo Item as an HTML list item
renderTodo :: Entity Todo -> Html ()
renderTodo (Entity todoId (Todo todo desc completed _ )) =
    li_ [class_ "p-4 flex justify-between items-center bg-white rounded shadow-md"] $ do
        div_ [class_ "flex-grow"] $ do
            span_ [class_ "font-semibold text-lg"] $ toHtml todo
            p_ [class_ "text-sm text-gray-600"] $ toHtml desc
            
        div_ [class_ "flex items-center gap-2"] $ do
            -- Toggle Button
            button_
                [ hxPost_ ("/todos/" <> toUrlPiece todoId <> "/toggle")
                , hxTarget_ "closest li"
                , hxSwap_ "outerHTML"
                , class_ $ "px-3 py-1 rounded " <> 
                    if completed 
                    then "bg-green-500 hover:bg-green-600" 
                    else "bg-yellow-500 hover:bg-yellow-600"
                ] $ toHtml (if completed then "Completed ✓" :: T.Text else "Mark Complete" :: T.Text)
                
            -- Delete Button
            button_
                [ hxDelete_ ("/todos/" <> toUrlPiece todoId)
                , hxTarget_ "closest li"
                , hxSwap_ "outerHTML"
                , class_ "bg-red-500 hover:bg-red-600 text-white px-3 py-1 rounded"
                ] "Delete"

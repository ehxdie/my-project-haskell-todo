{-# LANGUAGE OverloadedStrings #-}

module Pages.TodoPage (renderTodosPage, renderTodo) where

import Lucid
import Database.Persist.Sql (Entity(..))
import Models (Todo(..))
import Pages.Layout (baseLayout)
import Helpers.Htmx
import qualified Data.Text as T
import Web.HttpApiData (toUrlPiece)

renderTodosPage :: [Entity Todo] -> Html ()
renderTodosPage todos = baseLayout $ do
    -- Add logout button at the top
    div_ [class_ "flex justify-between items-center mb-6"] $ do
        h2_ [class_ "text-3xl font-bold text-center"] "Todo List"
        button_ [ class_ "bg-red-500 hover:bg-red-600 text-white px-4 py-2 rounded"
                , onclick_ "logout()"
                ] "Logout"
                
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
renderTodo (Entity todoId (Todo todo desc completed _)) =
    li_ [class_ "p-4 flex flex-col gap-4 bg-white rounded shadow-md"] $ do
        -- Display section
        div_ [class_ "flex justify-between items-center"] $ do
            div_ [class_ "flex-grow", id_ ("todo-display-" <> toUrlPiece todoId)] $ do
                span_ [class_ "font-semibold text-lg"] $ toHtml todo
                p_ [class_ "text-sm text-gray-600"] $ toHtml desc
        
            div_ [class_ "flex gap-2"] $ do
                button_
                    [ class_ "bg-blue-500 hover:bg-blue-600 text-white px-3 py-1 rounded"
                    , onclick_ $ "toggleEdit('" <> toUrlPiece todoId <> "')"
                    ] "Edit"
                
                button_
                    [ hxPost_ ("/todos/" <> toUrlPiece todoId <> "/toggle")
                    , hxTarget_ "closest li"
                    , hxSwap_ "outerHTML"
                    , class_ $ "px-3 py-1 rounded " <> 
                        if completed 
                        then "bg-green-500 hover:bg-green-600" 
                        else "bg-yellow-500 hover:bg-yellow-600"
                    ] $ toHtml (if completed then "Completed ✓" :: T.Text else "Mark Complete" :: T.Text)
                
                button_
                    [ hxDelete_ ("/todos/" <> toUrlPiece todoId)
                    , hxTarget_ "closest li"
                    , hxSwap_ "outerHTML"
                    , class_ "bg-red-500 hover:bg-red-600 text-white px-3 py-1 rounded"
                    ] "Delete"
        
        -- Edit form (initially hidden)
        form_ [ class_ "hidden flex flex-col gap-2", 
                id_ ("todo-edit-" <> toUrlPiece todoId),
                hxPut_ ("/todos/" <> toUrlPiece todoId),
                hxTarget_ "closest li",
                hxSwap_ "outerHTML"
              ] $ do
            input_ [ type_ "text"
                , name_ "todo"
                , value_ (T.pack todo)
                , class_ "border p-2 rounded w-full"
              ]
            input_ [ type_ "text"
                , name_ "description"
                , value_ (T.pack desc)
                , class_ "border p-2 rounded w-full"
              ]
            
            div_ [class_ "flex gap-2"] $ do
                button_ [ type_ "submit"
                    , class_ "bg-green-500 hover:bg-green-600 text-white px-3 py-1 rounded"
                    ] "Save"
                button_ [ type_ "button"
                    , class_ "bg-gray-500 hover:bg-gray-600 text-white px-3 py-1 rounded"
                    , onclick_ $ "toggleEdit('" <> toUrlPiece todoId <> "')"
                    ] "Cancel"

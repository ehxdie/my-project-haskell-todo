{-# LANGUAGE OverloadedStrings #-}

module Pages.Layout (baseLayout) where 

import Lucid

baseLayout :: Html () -> Html ()
baseLayout content = do
    doctypehtml_ $ do
        head_ $ do
            title_ "Todo App"
            link_ [rel_ "stylesheet", href_ "https://cdn.jsdelivr.net/npm/tailwindcss@2.2.19/dist/tailwind.min.css"]
            script_ [src_ "https://unpkg.com/htmx.org@1.8.4"] ("" :: String)
        body_ [class_ "bg-gray-100 min-h-screen p-6"] content

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
            script_ [type_ "text/javascript"] $ toHtmlRaw (unlines [
                "document.addEventListener('DOMContentLoaded', function() {",
                "    let authCookie = document.cookie.split('; ').find(row => row.startsWith('Authorization='));",
                "    if (authCookie) {",
                "        let token = authCookie.split('=')[1];",
                "        if (token.startsWith('Bearer ')) {",
                "            token = token.substring(7);",
                "        }",
                "        localStorage.setItem('jwt_token', token);",
                "    }",
                "",
                "   htmx.on('htmx:configRequest', function(evt) {",
                "               // Skip auth routes",
                "        if (evt.detail.path.startsWith('/auth') || ",
                "            evt.detail.path === '/login' ||",
                "            evt.detail.path === '/signup') {",
                "            return;",
                "        }",
                "",
                "        // Add token only for protected routes",
                "        const token = localStorage.getItem('jwt_token');",
                "        if (token) {",
                "            evt.detail.headers['Authorization'] = 'Bearer ' + token;",
                "        }",
                "    });",
                "});"
                ])

        body_ [class_ "bg-gray-100 min-h-screen p-6"] content

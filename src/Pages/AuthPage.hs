{-# LANGUAGE OverloadedStrings #-}

module Pages.AuthPage (renderAuthPage) where

import Lucid
import Pages.Layout

renderAuthPage :: Html ()
renderAuthPage = baseLayout $ do
    div_ [class_ "max-w-md mx-auto bg-white p-6 rounded shadow-md"] $ do
        h2_ [class_ "text-2xl font-bold"] "Login"

        form_ [class_ "space-y-4"] $ do
            input_ [name_ "email", type_ "email", placeholder_ "Email", class_ "w-full p-2 border rounded"]
            input_ [name_ "password", type_ "password", placeholder_ "Password", class_ "w-full p-2 border rounded"]
            button_ [class_ "bg-green-500 text-white px-4 py-2 rounded"] "Login"

        p_ [class_ "mt-4"] $ do
            "Don't have an account? "
            a_ [href_ "/register", class_ "text-blue-500"] "Sign up"

{-# LANGUAGE OverloadedStrings #-}

module Pages.AuthPage (renderAuthPage, renderLoginForm, renderSignupForm) where

import Lucid
import Pages.Layout
import Helpers.Htmx

renderAuthPage :: Html ()
renderAuthPage = baseLayout $ do
    div_ [class_ "max-w-md mx-auto bg-white p-6 rounded shadow-md"] $ do
        -- Tabs for switching between forms
        div_ [class_ "flex mb-6 border-b"] $ do
            -- For the login tab
            button_ [ class_ "py-2 px-4 text-blue-500 border-b-2 border-blue-500 font-medium"
                        , id_ "login-tab"
                        , hxGet_ "/auth/login"
                        , hxTarget_ "#auth-forms"
                        , hxSwap_ "innerHTML"
                        , hxTrigger_ "click"
                        , hxOn_ "click" "document.getElementById('signup-tab').classList.remove('text-blue-500', 'border-b-2', 'border-blue-500'); document.getElementById('signup-tab').classList.add('text-gray-500');"
                    ] "Login"

            -- For the signup tab
            button_ [ class_ "py-2 px-4 text-gray-500 font-medium"
                        , id_ "signup-tab"
                        , hxGet_ "/auth/signup"
                        , hxTarget_ "#auth-forms"
                        , hxSwap_ "innerHTML"
                        , hxTrigger_ "click"
                        , hxOn_ "click" "document.getElementById('login-tab').classList.remove('text-blue-500', 'border-b-2', 'border-blue-500'); document.getElementById('login-tab').classList.add('text-gray-500');"
                    ] "Sign Up"

        -- Forms container
        div_ [id_ "auth-forms"] $ renderLoginForm

-- Login Form
renderLoginForm :: Html ()
renderLoginForm = do
    form_ [ hxPost_ "/login"
          , hxTarget_ "body"  -- Target the whole body for redirection
          , class_ "space-y-4"
          , hxExt_ "json-enc"
          ] $ do
        -- Email field
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium"] "Email"
            input_ [ type_ "email"
                  , name_ "email"
                  , placeholder_ "you@example.com"
                  , class_ "w-full p-2 border rounded focus:ring-2 focus:ring-blue-500"
                  , required_ "required"
                  ]

        -- Password field
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium"] "Password"
            input_ [ type_ "password"
                  , name_ "password"
                  , placeholder_ "••••••••"
                  , class_ "w-full p-2 border rounded focus:ring-2 focus:ring-blue-500"
                  , required_ "required"
                  ]

        -- Submit button
        div_ [class_ "pt-2"] $
            button_ [ type_ "submit"
                   , class_ "w-full bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
                   ] "Login"

        -- Sign up link
        div_ [class_ "mt-4 text-center text-gray-600"] $ do
            "Don't have an account? "
            button_ [ class_ "text-blue-500 hover:underline"
                   , hxGet_ "/auth/signup"
                   , hxTarget_ "#auth-forms"
                   , hxSwap_ "innerHTML"
                   ] "Sign Up"

    -- Response message container
    div_ [id_ "response-message", class_ "mt-4 text-center"] ""

-- Sign Up Form
renderSignupForm :: Html ()
renderSignupForm = do
    form_ [ hxPost_ "/users"
          , hxTarget_ "#response-message"
          , hxSwap_ "outerHTML"
          , class_ "space-y-4"
          ] $ do
        -- Email field
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium"] "Email"
            input_ [ type_ "email"
                  , name_ "email"
                  , placeholder_ "you@example.com"
                  , class_ "w-full p-2 border rounded focus:ring-2 focus:ring-blue-500"
                  , required_ "required"
                  ]

        -- Password field
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium"] "Password"
            input_ [ type_ "password"
                  , name_ "passwordHash"  -- Match the field name with FromForm instance
                  , placeholder_ "••••••••"
                  , class_ "w-full p-2 border rounded focus:ring-2 focus:ring-blue-500"
                  , required_ "required"
                  ]

        -- Submit button
        div_ [class_ "pt-2"] $
            button_ [ type_ "submit"
                   , class_ "w-full bg-blue-500 text-white px-4 py-2 rounded hover:bg-blue-600"
                   ] "Create Account"

        -- Login link
        div_ [class_ "mt-4 text-center text-gray-600"] $ do
            "Already have an account? "
            button_ [ class_ "text-blue-500 hover:underline"
                   , hxGet_ "/auth/login"
                   , hxTarget_ "#auth-forms"
                   , hxSwap_ "innerHTML"
                   ] "Login"

    -- Response message container
    div_ [id_ "response-message", class_ "mt-4 text-center"] ""
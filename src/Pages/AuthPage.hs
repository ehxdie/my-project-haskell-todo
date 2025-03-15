{-# LANGUAGE OverloadedStrings #-}

module Pages.AuthPage (renderAuthPage, renderLoginForm, renderSignupForm, renderTabs) where

import Lucid
import Lucid.Base
import Pages.Layout
import Helpers.Htmx
import qualified Data.Text as T

-- Render Tabs with Improved Styling
renderTabs :: Bool -> Html ()
renderTabs isLogin = 
    let baseClasses = "py-2 px-4 font-medium transition duration-150 border-b-2 focus:outline-none focus:ring-2 focus:ring-blue-500"
        loginClasses = if isLogin
                       then baseClasses <> " text-blue-500 border-blue-500"
                       else baseClasses <> " text-gray-500 border-transparent hover:text-blue-500 hover:border-blue-500"
        signupClasses = if not isLogin
                        then baseClasses <> " text-blue-500 border-blue-500"
                        else baseClasses <> " text-gray-500 border-transparent hover:text-blue-500 hover:border-blue-500"
    in
    div_ [class_ "flex mb-6"] $ do
        button_ [ class_ loginClasses
                , id_ "login-tab"
                , hxGet_ "/auth/login"
                , hxTarget_ "#auth-forms"
                , hxSwap_ "innerHTML"
                , hxTrigger_ "click"
                , makeAttribute "onclick" "this.classList.add('text-blue-500', 'border-blue-500'); this.classList.remove('text-gray-500', 'border-transparent'); document.getElementById('signup-tab').classList.add('text-gray-500', 'border-transparent'); document.getElementById('signup-tab').classList.remove('text-blue-500', 'border-blue-500');"
                ] "Login"
        button_ [ class_ signupClasses
                , id_ "signup-tab"
                , hxGet_ "/auth/signup"
                , hxTarget_ "#auth-forms"
                , hxSwap_ "innerHTML"
                , hxTrigger_ "click"
                , makeAttribute "onclick" "this.classList.add('text-blue-500', 'border-blue-500'); this.classList.remove('text-gray-500', 'border-transparent'); document.getElementById('login-tab').classList.add('text-gray-500', 'border-transparent'); document.getElementById('login-tab').classList.remove('text-blue-500', 'border-blue-500');"
                ] "Sign Up"

-- Main Auth Page with Centered Layout
renderAuthPage :: Html ()
renderAuthPage = baseLayout $ do
    div_ [class_ "max-w-lg mx-auto bg-white p-8 rounded-lg shadow-lg"] $ do
        renderTabs True
        div_ [id_ "auth-forms"] $ renderLoginForm

-- Login Form with Enhanced Styling
renderLoginForm :: Html ()
renderLoginForm = do
    form_ [ hxPost_ "/login"
          , hxTarget_ "body"
          , class_ "space-y-4"
          , hxExt_ "json-enc"
          ] $ do
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium text-gray-700"] "Email"
            input_ [ type_ "email"
                   , name_ "email"
                   , placeholder_ "you@example.com"
                   , class_ "w-full p-2 border rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500 placeholder-gray-400 bg-gray-50"
                   , required_ "required"
                   ]
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium text-gray-700"] "Password"
            input_ [ type_ "password"  -- Corrected from "passwordHash"
                   , name_ "passwordHash"
                   , placeholder_ "••••••••"
                   , class_ "w-full p-2 border rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500 placeholder-gray-400 bg-gray-50"
                   , required_ "required"
                   ]
        div_ [class_ "pt-2"] $
            button_ [ type_ "submit"
                    , class_ "w-full bg-blue-500 text-white px-4 py-2 rounded-md hover:bg-blue-600 shadow-md font-semibold"
                    ] "Login"
        div_ [class_ "mt-4 text-center text-gray-600"] $ do
            "Don't have an account? "
            button_ [ class_ "text-blue-500 hover:underline"
                    , hxGet_ "/auth/signup"
                    , hxTarget_ "#auth-forms"
                    , hxSwap_ "innerHTML"
                    ] "Sign Up"
    div_ [id_ "response-message", class_ "mt-4 text-center"] ""
    script_ $ T.pack "var loginTab = document.getElementById('login-tab'); var signupTab = document.getElementById('signup-tab'); loginTab.classList.add('text-blue-500', 'border-blue-500'); loginTab.classList.remove('text-gray-500', 'border-transparent'); signupTab.classList.add('text-gray-500', 'border-transparent'); signupTab.classList.remove('text-blue-500', 'border-blue-500');"

-- Signup Form with Enhanced Styling
renderSignupForm :: Html ()
renderSignupForm = do
    form_ [ hxPost_ "/users"
          , hxTarget_ "#auth-forms"
          , hxSwap_ "innerHTML"
          , class_ "space-y-4"
          ] $ do
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium text-gray-700"] "Email"
            input_ [ type_ "email"
                   , name_ "email"
                   , placeholder_ "you@example.com"
                   , class_ "w-full p-2 border rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500 placeholder-gray-400 bg-gray-50"
                   , required_ "required"
                   ]
        div_ [class_ "space-y-1"] $ do
            label_ [class_ "text-sm font-medium text-gray-700"] "Password"
            input_ [ type_ "password"
                   , name_ "passwordHash"
                   , placeholder_ "••••••••"
                   , class_ "w-full p-2 border rounded-md focus:ring-2 focus:ring-blue-500 focus:border-blue-500 placeholder-gray-400 bg-gray-50"
                   , required_ "required"
                   ]
        div_ [class_ "pt-2"] $
            button_ [ type_ "submit"
                    , class_ "w-full bg-blue-500 text-white px-4 py-2 rounded-md hover:bg-blue-600 shadow-md font-semibold"
                    ] "Create Account"
        div_ [class_ "mt-4 text-center text-gray-600"] $ do
            "Already have an account? "
            button_ [ class_ "text-blue-500 hover:underline"
                    , hxGet_ "/auth/login"
                    , hxTarget_ "#auth-forms"
                    , hxSwap_ "innerHTML"
                    ] "Login"
    div_ [id_ "response-message", class_ "mt-4 text-center"] ""
    script_ $ T.pack "var signupTab = document.getElementById('signup-tab'); var loginTab = document.getElementById('login-tab'); signupTab.classList.add('text-blue-500', 'border-blue-500'); signupTab.classList.remove('text-gray-500', 'border-transparent'); loginTab.classList.add('text-gray-500', 'border-transparent'); loginTab.classList.remove('text-blue-500', 'border-blue-500');"
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Handlers.Auth
    ( getAuthPage
    , getLoginForm
    , getSignupForm
    , loginUser
    ) where

import Servant
import Models (User(..), userEmail, userPasswordHash,todoUserId, EntityField(..))
import Database.Persist.Sql (Entity(..), ConnectionPool, selectList)
import Database (runDB)
import Pages.AuthPage (renderAuthPage, renderLoginForm, renderSignupForm)
import Lucid (Html)
import qualified Database.Persist as P
import Config.PasswordHashing (verifyPassword)
import Handlers.Todo (getTodos)
import Pages.TodoPage (renderTodosPage)
import qualified Data.ByteString.Lazy.Char8 as BL
import Servant.HTML.Lucid (HTML)
import Config.JwtAuth (generateJWT)
import Config.AppConfig (getJwtSecret)
import Data.Text.Encoding (decodeUtf8)
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Servant.API (Headers, Header, addHeader)

-- Auth Page Handlers
getAuthPage :: ConnectionPool -> Handler (Html ())
getAuthPage _ = return renderAuthPage

getLoginForm :: Handler (Html ())
getLoginForm = return renderLoginForm

getSignupForm :: Handler (Html ())
getSignupForm = return renderSignupForm

loginUser :: ConnectionPool -> User -> Handler (Headers '[Header "Set-Cookie" Text] (Html ()))

loginUser pool user = do
    users <- runDB (P.selectList [UserEmail P.==. userEmail user] []) pool
    case users of
        (Entity userId dbUser : _) -> 
            if verifyPassword (userPasswordHash user) (userPasswordHash dbUser)
                then do
                    -- Generate JWT token
                    secret <- liftIO getJwtSecret
                    token <- liftIO $ generateJWT secret (T.pack $ userEmail dbUser)
                    -- Get all todos associated with the user's ID
                    todos <- runDB (selectList [TodoUserId P.==. userId] []) pool
                    -- Create a cookie value (using OverloadedStrings, literals are Text)
                    let cookieValue = "Authorization=" <> token <> "; Path=/; HttpOnly"
                    -- Return the page with token as a cookie header
                    return $ addHeader cookieValue (renderTodosPage todos)
                else throwError err401 { errBody = BL.pack "Invalid credentials" }
        [] -> throwError err401 { errBody = BL.pack "Invalid credentials" }

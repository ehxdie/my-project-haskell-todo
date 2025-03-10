{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.Auth
    ( getAuthPage
    , getLoginForm
    , getSignupForm
    , loginUser
    ) where

import Servant
import Models (User(..), userEmail, userPasswordHash, EntityField(..))
import Database.Persist.Sql (Entity(..), ConnectionPool)
import Database (runDB)
import Pages.AuthPage (renderAuthPage, renderLoginForm, renderSignupForm)
import Lucid (Html)
import qualified Database.Persist as P
import Config.PasswordHashing (verifyPassword)
import Handlers.Todo (getTodos)
import Pages.TodoPage (renderTodosPage)
import qualified Data.ByteString.Lazy.Char8 as BL
import Servant.HTML.Lucid (HTML)

-- Auth Page Handlers
getAuthPage :: ConnectionPool -> Handler (Html ())
getAuthPage _ = return renderAuthPage

getLoginForm :: Handler (Html ())
getLoginForm = return renderLoginForm

getSignupForm :: Handler (Html ())
getSignupForm = return renderSignupForm

loginUser :: ConnectionPool -> User -> Handler (Html ())
loginUser pool user = do
    users <- runDB (P.selectList [UserEmail P.==. userEmail user] []) pool
    case users of
        (Entity _ dbUser : _) -> 
            if verifyPassword (userPasswordHash user) (userPasswordHash dbUser)
                then do
                    todos <- getTodos pool
                    return $ renderTodosPage todos
                else throwError err401 { errBody = BL.pack "Invalid credentials" }
        [] -> throwError err401 { errBody = BL.pack "Invalid credentials" }

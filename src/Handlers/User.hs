{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Handlers.User
    ( getUsers
    , createUser
    ) where

import Servant
import Models (Todo(..), User(..), userEmail, userPasswordHash, EntityField(..))
import Database.Persist.Sql (Entity(..), ConnectionPool, insertEntity, selectList, replace, delete, get)
import Database (runDB)
import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (Key)
import Pages.TodoPage (renderTodosPage, renderTodo)
import Pages.AuthPage (renderAuthPage)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)
import Control.Monad.Logger (runStdoutLoggingT, logInfo, logDebug)
import qualified Data.Text as T
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Database.Persist as P
import Data.Text (Text)
import Pages.AuthPage (renderLoginForm, renderSignupForm)
import Control.Monad (void, when)
import Config.PasswordHashing (hashPassword, verifyPassword)



-- USER HANDLERS
getUsers :: ConnectionPool -> Handler [Entity User]
getUsers pool = runDB (selectList [] []) pool

-- CREATE USER (Signup)
createUser :: ConnectionPool -> User -> Handler (Html ())
createUser pool user = do
    liftIO $ runStdoutLoggingT $ do
        $(logInfo) $ T.pack "Received signup request"
        $(logDebug) $ T.pack "User email: " <> T.pack (userEmail user)
    
    -- Validate email and password
    when (T.null $ T.pack $ userEmail user) $ 
        throwError err400 { errBody = BL.pack "Email cannot be empty" }
    when (T.null $ T.pack $ userPasswordHash user) $ 
        throwError err400 { errBody = BL.pack "Password cannot be empty" }
    
    -- Check if user already exists
    existing <- runDB (P.selectList [UserEmail P.==. userEmail user] []) pool
    case existing of
        (_:_) -> throwError err409 { errBody = BL.pack "User already exists" }
        [] -> do
            -- Hash the password before storing
            hashedPassword <- liftIO $ hashPassword (userPasswordHash user)
            let newUser = user { userPasswordHash = hashedPassword }
            result <- runDB (insertEntity newUser) pool
            return renderLoginForm




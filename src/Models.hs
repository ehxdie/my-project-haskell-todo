{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-} 
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Models where
  
import Database.Persist.TH
import Database.Persist
import Database.Persist.Sql
import Data.Aeson
import qualified Data.Aeson.TH as Aeson
import GHC.Generics (Generic)
import Data.Char (toLower)
import Web.FormUrlEncoded hiding (fieldLabelModifier)  -- Hide the conflicting name
import Web.HttpApiData
import Control.Monad
import Data.Maybe (fromMaybe)

-- Persistent database schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User
    email String
    passwordHash String
    UniqueEmail email  -- Add unique constraint on email
    deriving Show Generic
  
  Todo
    todo String
    description String
    completed Bool
    userId UserId   -- Foreign key reference to User
    deriving Show Generic
|]

-- Custom JSON options to handle todoTodo, todoDescription, todoCompleted field names
customOptions = Aeson.defaultOptions {
    Aeson.fieldLabelModifier = drop 4  -- Drop the "todo" prefix from field names
}

-- USERS

-- JSON instance for User using custom options
instance ToJSON User where
    toJSON (User email _) = object [ "email" .= email ]  -- Do not expose password hash

instance FromJSON User where
    parseJSON = withObject "User" $ \v -> 
        User <$> v .: "email"
             <*> v .: "passwordHash"

-- JSON instance for Entity User
instance ToJSON (Entity User) where
    toJSON (Entity key value) =
        object [ "id" .= key
               , "email" .= userEmail value
               , "passwordHash" .= userPasswordHash value
               ]

instance FromJSON (Entity User) where
    parseJSON = withObject "Entity User" $ \v -> do
        key <- v .: "id"
        email <- v .: "email"
        passwordHash <- v .: "passwordHash"
        return $ Entity key (User email passwordHash)

instance FromForm User where
    fromForm f = User
        <$> parseUnique "email" f
        <*> parseUnique "passwordHash" f  -- Match the field name from the form
  -- Try both field names

-- TODO

-- Setting up custom type for todoForm for form submissions
data TodoForm = TodoForm {
    formTodo :: String,        -- Changed from todo to formTodo
    formDescription :: String, -- Changed from description to formDescription
    formCompleted :: Bool      -- Changed from completed to formCompleted
} deriving (Show, Generic)

-- Setting up coversion function from todoForm to Todo type
todoFromForm :: UserId -> TodoForm -> Todo
todoFromForm userId form = Todo 
    { todoTodo = formTodo form           -- Now uses formTodo
    , todoDescription = formDescription form  -- Now uses formDescription
    , todoCompleted = formCompleted form     -- Now uses formCompleted
    , todoUserId = userId
    }

-- JSON instance for Todo 
instance ToJSON Todo where
    toJSON (Todo t d c u) = object [
        "todo" .= t,
        "description" .= d,
        "completed" .= c,
        "userId" .= u
      ]

-- Handles parsing of Json todo data into TodoForm
instance FromJSON TodoForm where
    parseJSON = withObject "TodoForm" $ \v -> TodoForm
        <$> v .: "todo"
        <*> v .: "description" 
        <*> v .:? "completed" .!= False

-- Handles parsing of FormUrlEncoded todo data into TodoForm
instance FromForm TodoForm where
    fromForm f = TodoForm
        <$> parseUnique "todo" f
        <*> parseUnique "description" f
        <*> (fromMaybe False <$> parseMaybe "completed" f)


instance ToJSON (Entity Todo) where
    toJSON (Entity key value) =
        object [ "id" .= key
               , "todo" .= todoTodo value
               , "description" .= todoDescription value
               , "completed" .= todoCompleted value
               , "userId" .= todoUserId value
               ]

instance FromJSON (Entity Todo) where
    parseJSON = withObject "Entity Todo" $ \v -> do
        key <- v .: "id"
        todo <- v .: "todo"
        description <- v .: "description"
        completed <- v .: "completed"
        userId <- v .: "userId"
        return $ Entity key (Todo todo description completed userId)
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

module Models (
    Todo(..),
    User(..),         -- ✅ Export Todo entity
    migrateAll        -- ✅ Export migrateAll for migrations
) where
  
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
        <*> parseUnique "password" f  -- The field name in the form will be "password"

-- TODO

-- Use custom options for JSON instances
instance ToJSON Todo where
    toJSON (Todo t d c u) = object [
        "todo" .= t,
        "description" .= d,
        "completed" .= c,
        "userId" .= u
      ]

instance FromJSON Todo where
    parseJSON = withObject "Todo" $ \v -> Todo
        <$> v .: "todo"
        <*> v .: "description" 
        <*> v .:? "completed" .!= False
        <*> v .: "userId"

instance FromForm Todo where
    fromForm f = Todo
        <$> parseUnique "todo" f
        <*> parseUnique "description" f
        <*> (fromMaybe False <$> parseMaybe "completed" f)
        <*> parseUnique "userId" f

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
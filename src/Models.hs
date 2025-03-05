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
    Todo(..),         -- ✅ Export Todo entity
    migrateAll        -- ✅ Export migrateAll for migrations
) where
  
import Database.Persist.TH
import Database.Persist
import Database.Persist.Sql
import Data.Aeson
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import GHC.Generics (Generic)
import Data.Char (toLower)

-- Persistent database schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Todo
    todo String
    description String
    completed Bool
    deriving Show Generic
|]

-- Custom JSON options to handle todoTodo, todoDescription, todoCompleted field names
customOptions = defaultOptions {
    fieldLabelModifier = drop 4  -- Drop the "todo" prefix from field names
}

-- ✅ Use custom options for JSON instances
instance ToJSON Todo where
    toJSON (Todo t d c) = object [
        "todo" .= t,
        "description" .= d,
        "completed" .= c
      ]

instance FromJSON Todo where
    parseJSON = withObject "Todo" $ \v -> Todo
        <$> v .: "todo"
        <*> v .: "description" 
        <*> v .: "completed"

-- ✅ JSON instance for Entity Todo
instance ToJSON (Entity Todo) where
    toJSON (Entity key value) =
        object [ "id" .= key
               , "todo" .= todoTodo value
               , "description" .= todoDescription value
               , "completed" .= todoCompleted value
               ]

instance FromJSON (Entity Todo) where
    parseJSON = withObject "Entity Todo" $ \v -> do
        key <- v .: "id"
        todo <- v .: "todo"
        description <- v .: "description"
        completed <- v .: "completed"
        return $ Entity key (Todo todo description completed)
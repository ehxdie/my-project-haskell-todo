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
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic) 

-- Persistent database schema
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|  
Todo
    todo String
    description String
    completed Bool
    deriving Show Generic
|]

instance ToJSON Todo
instance FromJSON Todo

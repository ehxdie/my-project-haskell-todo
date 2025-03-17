{-# LANGUAGE OverloadedStrings #-}

module Config.AppConfig  where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import Configuration.Dotenv (loadFile, defaultConfig)  -- Load .env files
import System.Environment (getEnv)                     -- Get environment variables
import Data.Text (Text, pack)                          -- Convert String to Text
import Data.Text.Encoding (encodeUtf8)                 -- Convert Text to ByteString
import Database.Persist.Postgresql (ConnectionString)  -- Define connection string type

getJwtSecret :: IO ByteString
getJwtSecret = TE.encodeUtf8 . T.pack <$> getEnv "JWT_SECRET"

loadEnv :: IO ()
loadEnv = loadFile defaultConfig

connStr :: IO ConnectionString
connStr = do
  host <- pack <$> getEnv "DB_HOST"
  port <- pack <$> getEnv "DB_PORT"
  user <- pack <$> getEnv "DB_USER"
  password <- pack <$> getEnv "DB_PASSWORD"
  name <- pack <$> getEnv "DB_NAME"
  return $ encodeUtf8 $ "host=" <> host <> " dbname=" <> name <> " user=" <> user <> " password=" <> password <> " port=" <> port
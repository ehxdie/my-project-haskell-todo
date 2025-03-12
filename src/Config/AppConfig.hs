{-# LANGUAGE OverloadedStrings #-}

module Config.AppConfig (getJwtSecret) where

import System.Environment (getEnv)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)

getJwtSecret :: IO ByteString
getJwtSecret = TE.encodeUtf8 . T.pack <$> getEnv "JWT_SECRET"

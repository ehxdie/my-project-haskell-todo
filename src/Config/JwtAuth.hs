{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Config.JwtAuth where

import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import GHC.Generics
import qualified Web.JWT as JWT
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as B
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Monad.IO.Class (liftIO)
import Servant
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Control.Applicative ((<|>))

data JWTPayload = JWTPayload 
    { email :: Text
    , exp_  :: Integer  -- Note: this is for your custom type, not the JWT exp
    } deriving (Show, Generic)

instance ToJSON JWTPayload
instance FromJSON JWTPayload

data AuthenticatedUser = AuthenticatedUser 
    { userEmail :: Text 
    } deriving (Show)

generateJWT :: B.ByteString -> Text -> IO Text
generateJWT secret userEmail = do
    now <- getCurrentTime
    let expiryTime = addUTCTime (3600 * 24) now  -- 24 hours from now
        signer = JWT.hmacSecret (T.pack $ B.unpack secret)
        expirySeconds = utcTimeToPOSIXSeconds expiryTime
        claims = JWT.JWTClaimsSet
            { JWT.iss = Nothing
            , JWT.sub = Nothing
            , JWT.aud = Nothing
            , JWT.exp = JWT.numericDate expirySeconds
            , JWT.nbf = Nothing
            , JWT.iat = Nothing
            , JWT.jti = Nothing
            , JWT.unregisteredClaims = JWT.ClaimsMap $ Map.fromList
                [("email", toJSON userEmail)]
            }
    return $ JWT.encodeSigned signer mempty claims

verifyJWT :: B.ByteString -> Text -> Maybe AuthenticatedUser
verifyJWT secret token = do
    let signer = JWT.hmacSecret (T.pack $ B.unpack secret)
    jwt <- JWT.decodeAndVerifySignature (JWT.toVerify signer) token
    let claims = JWT.claims jwt
        JWT.ClaimsMap unregistered = JWT.unregisteredClaims claims
    emailVal <- Map.lookup "email" unregistered
    case fromJSON emailVal of
        Success email -> pure $ AuthenticatedUser email
        Error _ -> Nothing

type AuthHeader = Header "Authorization" Text

requireAuth :: B.ByteString -> Maybe Text -> Handler AuthenticatedUser
requireAuth secret mAuthHeader = case mAuthHeader of
    Nothing -> throwError err401
    Just token -> case verifyJWT secret (stripBearer token) of
        Nothing -> throwError err401 { errBody = "Invalid token" }
        Just user -> return user
  where
    stripBearer :: Text -> Text
    stripBearer t = fromMaybe t $ 
        T.stripPrefix "Bearer " t <|>  
        T.stripPrefix "Authorization=" t
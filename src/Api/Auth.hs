{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Auth (AuthAPI) where

import Servant
import Models (User(..))
import Database.Persist (Entity)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)
import Servant.API (Headers, Header)
import Data.Text (Text) 

type AuthAPI = 
       "auth" :> Get '[HTML] (Html ())  -- Render auth page
  :<|> "auth" :> "login" :> Get '[HTML] (Html ())  -- Get login form
  :<|> "auth" :> "signup" :> Get '[HTML] (Html ())  -- Get signup form
  :<|> "login" :> ReqBody '[JSON, FormUrlEncoded] User :> Post '[HTML] (Headers '[Header "Set-Cookie" Text] (Html ()))  -- Post request for user login

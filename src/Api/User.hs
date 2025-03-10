{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}


module Api.User (UserAPI) where

import Servant
import Models (User(..))
import Database.Persist (Entity)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)

type UserAPI = 
       "users" :> Get '[JSON] [Entity User]  -- List users
  :<|> "users" :> ReqBody '[JSON, FormUrlEncoded] User :> Post '[HTML] (Html ())  -- Create user

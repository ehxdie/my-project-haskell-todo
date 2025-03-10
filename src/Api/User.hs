{-# LANGUAGE DataKinDs #-}
{-# LANGUAGE TypeOperators #-}

module Api.User (UserAPI) where

import Servant
import Models (User(..))
import Database.Persist (Entity)

type UserAPI = 
       "users" :> Get '[JSON] [Entity User]  -- List users
  :<|> "users" :> ReqBody '[JSON, FormUrlEncoded] User :> Post '[JSON] (Entity User)  -- Create user

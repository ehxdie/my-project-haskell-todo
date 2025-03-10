{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api 
    ( api
    , API
    , AuthAPI
    , TodoAPI
    , UserAPI
    ) where

import Servant
import Api.Auth (AuthAPI)
import Api.Todo (TodoAPI)
import Api.User (UserAPI)

-- Combined API type
type API = AuthAPI 
      :<|> TodoAPI 
      :<|> UserAPI

api :: Proxy API
api = Proxy

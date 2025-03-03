{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (api,API) where

import Servant
import Models(Todo(..))

type API =
       "todos" :> Get '[JSON] [Entity Todo]  -- Retrieve all todos
  :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] (Entity Todo)  -- Create a todo
  :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON] Todo :> Put '[JSON] (Entity Todo)  -- Update a todo
  :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] NoContent  -- Delete a todo
  
api :: Proxy API
api = Proxy
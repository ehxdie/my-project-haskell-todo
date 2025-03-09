{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (api, API) where

import Servant
import Models (Todo(..))
import Database.Persist (Entity, Key)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)

type API =
       "todos" :> Get '[JSON] [Entity Todo]  -- Retrieve all todos as JSON
  :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[HTML] (Html ())  -- Create a todo, return HTML fragment
  :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] (Entity Todo)  -- Update a todo
  :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] NoContent  -- Delete a todo
  :<|> "todos" :> "html" :> Get '[HTML] (Html ())  -- Render the full todos page as HTML
  :<|> "auth" :> Get '[HTML] (Html ())  -- Render the auth page as HTML

api :: Proxy API
api = Proxy

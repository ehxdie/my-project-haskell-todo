{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (api,API) where

import Servant
import Models(Todo(..))
import Database.Persist (Entity, Key)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)

type API =
       "todos" :> Get '[JSON] [Entity Todo]  -- Retrieve all todos
  :<|> "todos" :> ReqBody '[JSON] Todo :> Post '[JSON] (Entity Todo)  -- Create a todo
  :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON] Todo :> Put '[JSON] (Entity Todo)  -- Update a todo
  :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] NoContent  -- Delete a todo
  :<|> "todos" :> "html" :> Get '[HTML] (Html ())  -- ✅ Todo page (HTML)
  :<|> "auth" :> Get '[HTML] (Html ())  -- ✅ Auth page (HTML)

api :: Proxy API
api = Proxy
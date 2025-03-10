{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Todo (TodoAPI) where

import Servant
import Models (Todo(..))
import Database.Persist (Entity, Key)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)

type TodoAPI = 
       "todos" :> Get '[JSON] [Entity Todo]  -- List todos
  :<|> "todos" :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[HTML] (Html ())  -- Create todo
  :<|> "todos" :> Capture "id" (Key Todo) :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] (Entity Todo)  -- Update todo
  :<|> "todos" :> Capture "id" (Key Todo) :> Delete '[JSON] NoContent  -- Delete todo
  :<|> "todos" :> Capture "id" (Key Todo) :> "toggle" :> Post '[HTML] (Html ())  -- Toggle todo
  :<|> "todos" :> "html" :> Get '[HTML] (Html ())  -- Render todos page

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Todo (TodoAPI) where

import Servant
import Models (Todo(..))
import Database.Persist (Entity, Key)
import Lucid (Html)
import Servant.HTML.Lucid (HTML)
import Data.Text (Text)

type AuthHeader = Header "Authorization" Text

type TodoAPI = 
       "todos" :> AuthHeader :> Get '[JSON] [Entity Todo]  -- Get all todos (protected)
  :<|> "todos" :> AuthHeader :> ReqBody '[JSON, FormUrlEncoded] Todo :> Post '[HTML] (Html ())  -- Create a todo (protected)
  :<|> "todos" :> AuthHeader :> Capture "id" (Key Todo) :> ReqBody '[JSON, FormUrlEncoded] Todo :> Put '[JSON] (Entity Todo)  -- Update a todo (protected)
  :<|> "todos" :> AuthHeader :> Capture "id" (Key Todo) :> Delete '[JSON] NoContent  -- Delete a todo (protected)
  :<|> "todos" :> AuthHeader :> Capture "id" (Key Todo) :> "toggle" :> Post '[HTML] (Html ())  -- Toggle a todo (protected)
  :<|> "todos" :> AuthHeader :> "html" :> Get '[HTML] (Html ())  -- Render todos page (protected)


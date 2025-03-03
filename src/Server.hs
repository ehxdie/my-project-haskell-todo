{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server(app, runServer) where

import Servant
import Api
import Models (User(..))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Handlers (getUser, getUserById,postUser, updateUser, deleteUser)


server :: MVar [User] -> Server API
server usersVar = getUser usersVar :<|> getUserById usersVar :<|> postUser usersVar :<|> updateUser usersVar  :<|> deleteUser usersVar 



app :: MVar [User] -> Application
app usersVar = serve api (server usersVar)

runServer :: IO ()
runServer = do
    usersVar <- newMVar [User 1 "Isaac" "isaac@gmail.com"]
    putStrLn "Running server on port 8080"
    run 8080 (app usersVar)
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Handlers (getUser, getUserById, postUser, deleteUser, updateUser) where

import Servant
import Models(User(..))
import Control.Monad.IO.Class(liftIO)
import Control.Concurrent.MVar (MVar, readMVar, modifyMVar, modifyMVar_)
import Data.Maybe (listToMaybe)

getUser :: MVar [User] -> Handler [User]
getUser usersVar = liftIO $ readMVar usersVar

getUserById :: MVar [User] -> Int -> Handler User
getUserById usersVar uId = do
  users <- liftIO $ readMVar usersVar
  let maybeUser = listToMaybe $ filter (\u -> userId u == uId) users
  case maybeUser of
    Nothing -> throwError err404 {errBody = "User not found"}
    Just user -> return user

postUser :: MVar [User] -> User -> Handler [User]
postUser usersVar user = liftIO $ modifyMVar usersVar $ \users -> 
    let newId = if null users then 1 else userId (last users) + 1
        newUser = user {userId = newId}
        updatedUsers = users ++ [newUser]
    in return (updatedUsers, updatedUsers)

deleteUser :: MVar [User] -> Int -> Handler NoContent
deleteUser usersVar uId = liftIO $ do
    modifyMVar_ usersVar $ \users -> 
        return $ filter (\u -> userId u /= uId) users
    return NoContent

updateUser :: MVar [User] -> Int -> User -> Handler User
updateUser usersVar uId updatedUser = do
    -- First modify the users list
    liftIO $ modifyMVar_ usersVar $ \users ->
        return $ map (\u -> if userId u == uId 
                           then updatedUser { userId = uId } 
                           else u) users
    
    -- Then read the updated list to find the user
    users <- liftIO $ readMVar usersVar
    let maybeUser = listToMaybe $ filter (\u -> userId u == uId) users
    
    -- Handle the case where user might not exist
    case maybeUser of
        Nothing -> throwError err404 {errBody = "User not found"}
        Just user -> return user
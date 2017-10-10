#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson (FromJSON, ToJSON, Value, (.=), object)
import GHC.Generics
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status  (statusCode, internalServerError500)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as NL8

data User = User { userId :: Int, userName :: String } deriving (Generic, Show)

instance ToJSON User

instance FromJSON User

bob :: User
bob = User { userId = 1, userName = "bob" }

jenny :: User
jenny = User { userId = 2, userName = "jenny" }

routes :: ScottyM ()
routes = do
    get "/v1/posts" $ do
        fb_id <- param "fb_id"
        fb_token <- param "fb_token"
        post <- liftIO(getPosts fb_id fb_token)
        json $ post

getPosts :: String -> String -> IO String
getPosts fb_id fb_token = do
    initReq <- parseRequest ("https://graph.facebook.com/" ++ fb_id ++ "/feed")
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fb_token)) ]
                }
    response <- httpJSON req
    return (show (getResponseBody response :: Value))

getPosts2 :: String -> String -> IO Value
getPosts2 fb_id fb_token = do
    initReq <- parseRequest ("https://graph.facebook.com/" ++ fb_id ++ "/feed")
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fb_token)) ]
                }
    response <- httpJSON req
    return (getResponseBody response :: Value)

-- putStrLn $ 

main = do
    putStrLn "Starting Server on port 5000"
    post <- getPosts "asdfsdf" "adfsa"
    putStrLn post
    scotty 5000 routes

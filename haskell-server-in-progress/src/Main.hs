#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson
import GHC.Generics
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status  (statusCode)

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as NL8

data User = User { userId :: Int, userName :: String } deriving (Generic, Show)

instance ToJSON User where
    toEncoding = genericToEncoding defaultOptions

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
        text ("Add call to getPosts" <> fb_id <> "!" <> fb_token)

getPosts :: String -> String -> IO ()
getPosts fb_id fb_token = do
    initReq <- parseRequest ("https://graph.facebook.com/" ++ fb_id ++ "/feed")
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fb_token)) ]
                }
    response <- httpLBS req
    putStrLn $ "The status code was: " ++
        show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    L8.putStrLn $ getResponseBody response


main = do
    putStrLn "Starting Server on port 5000"
    scotty 5000 routes

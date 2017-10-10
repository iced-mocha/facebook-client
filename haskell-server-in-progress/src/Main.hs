#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Data.Monoid ((<>))
import Data.Aeson hiding (json)
import Data.Aeson.Types hiding (json)
import Data.HashMap.Strict
import GHC.Generics
import Control.Monad
import Control.Monad.Trans
import Control.Monad.List
import Network.HTTP.Simple
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

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
        postsRes <- liftIO(getPosts fb_id fb_token)
        let res = fst postsRes
        let code = snd postsRes
        status $ mkStatus code (NL8.pack "") -- todo: better way?
        if code == 200
            then json $ lookupDefault "" "data" res
            else json $ res

getPosts :: String -> String -> IO (Object, Int)
getPosts fb_id fb_token = do
    initReq <- parseRequest ("https://graph.facebook.com/" ++ fb_id ++ "/feed")
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fb_token)) ]
                }
    response <- httpJSON req
    let code = getResponseStatusCode response
    return (getResponseBody response :: Object, code)

-- putStrLn $ 

main = do
    putStrLn "Starting Server on port 5000"
    scotty 5000 routes

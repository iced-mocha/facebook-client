#!/usr/bin/env stack

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Web.Scotty
import Data.Maybe
import Data.Data
import Data.Text.Lazy
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

data FbPosts = FbPosts
    { posts :: [FbPost]
    , paging :: FbPaging
    } deriving (Show, Generic)

instance FromJSON FbPosts where
    parseJSON (Object v) =
        FbPosts         <$>
        (v .: "data")   <*>
        (v .: "paging")

instance ToJSON FbPosts

data FbPost = FbPost
    { msg :: String
    , story :: String
    , created_time :: String
    , id :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data FbPaging = FbPaging
    { previous :: String
    , next :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data FbErrorContent = FbErrorContent
    { message :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data FbError = FbError 
    { error :: FbErrorContent
    } deriving (Show, Generic, ToJSON, FromJSON)

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
            then json $ parsePosts res
            else json $ object [ "error" .= (getErrorMessage $ parseError res) ]

getErrorMessage :: FbError -> String
getErrorMessage FbError{Main.error=err} = message err

parsePosts :: String -> FbPosts
parsePosts post = 
    let parsed = Data.Aeson.decode $ L8.pack post :: Maybe FbPosts
    in  if isJust parsed
            then fromJust parsed
            else FbPosts [] $ FbPaging "" ""

parseError :: String -> FbError
parseError err =
    let parsed = Data.Aeson.decode $ L8.pack err :: Maybe FbError
    in  if isJust parsed
            then fromJust parsed
            else FbError $ FbErrorContent  ""

getPosts :: String -> String -> IO (String, Int)
getPosts fb_id fb_token = do
    initReq <- parseRequest ("https://graph.facebook.com/" ++ fb_id ++ "/feed")
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fb_token)) ]
                }
    response <- httpLBS req
    let code = getResponseStatusCode response
    return (L8.unpack $ getResponseBody response, code)

-- putStrLn $ 

main = do
    putStrLn "Starting Server on port 5000"
    scotty 5000 routes

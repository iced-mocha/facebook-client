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

facebookPlatform = "facebook"

data User = User { userId :: Int, userName :: String } deriving (Generic, Show)

data Posts = Posts
    { posts :: [Post]
    , nexturl :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data Post = Post
    { id :: String
    , date :: String
    , author :: String
    , profileimg :: String
    , title :: String
    , content :: String
    , heroimg :: String
    , postlink :: String
    , platform :: String
    } deriving (Show, Generic, ToJSON, FromJSON)

data FbPosts = FbPosts
    { fb_posts :: [FbPost]
    , paging :: FbPaging
    } deriving (Show, Generic, ToJSON)

instance FromJSON FbPosts where
    parseJSON (Object v) =
        FbPosts <$> v .: "data"
                <*> v .: "paging"
    parseJSON _ = mzero

data FbPost = FbPost
    { fb_id :: String
    , fb_created_time :: String
    , fb_story :: Maybe String
    , fb_message :: Maybe String
    , fb_picture :: Maybe String
    , fb_caption :: Maybe String
    , fb_link :: Maybe String
    } deriving (Show, Generic, ToJSON)

instance FromJSON FbPost where
    parseJSON (Object v) =
        FbPost <$> v .: "id"
               <*> v .: "created_time"
               <*> v .:? "story"
               <*> v .:? "message"
               <*> v .:? "picture"
               <*> v .:? "caption"
               <*> v .:? "link"
    parseJSON _ = mzero

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

-- TODO: If there is an HttpException, like a timeout, it will not be caught!!
routes :: ScottyM ()
routes = do
    get "/v1/posts" $ do
        fb_id <- param "fb_id"
        fb_token <- param "fb_token"
        postsRes <- liftIO(getPosts fb_id fb_token)
        let res = fst postsRes
        let code = snd postsRes
        status $ mkStatus code (NL8.pack "")
        if code == 200
            then json $ makePostsGeneric $ parsePosts res
            else json $ object [ "error" .= (getErrorMessage $ parseError res) ]

getErrorMessage :: FbError -> String
getErrorMessage FbError{Main.error=err} = message err

makePostsGeneric :: FbPosts -> Posts
makePostsGeneric fbPosts =
    Posts (Prelude.map makePostGeneric $ fb_posts fbPosts) (Main.next $ paging fbPosts)

trimOffsetFromTime :: String -> String
trimOffsetFromTime time = Prelude.take (Prelude.length time - 5) time

makePostGeneric :: FbPost -> Post
makePostGeneric fbPost = 
    Post { Main.id = fb_id fbPost
         , author = ""
         , profileimg = ""
         , date = (trimOffsetFromTime $ fb_created_time fbPost) ++ "Z"
         , title = (fromMaybe "" (fb_story fbPost))
         , content = (fromMaybe "" (fb_message fbPost))
         , heroimg = (fromMaybe "" (fb_picture fbPost))
         , postlink = (fromMaybe "" (fb_link fbPost))
         , platform = facebookPlatform }

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
    initReq <- parseRequest ("https://graph.facebook.com/" ++ fb_id ++ "/feed?fields=link,message,description,caption,story,created_time,picture")
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fb_token)) ]
                }
    response <- httpLBS req
    let code = getResponseStatusCode response
    return (L8.unpack $ getResponseBody response, code)

main = do
    putStrLn "Starting Server on port 5000"
    scotty 5000 routes

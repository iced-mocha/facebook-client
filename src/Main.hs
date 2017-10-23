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
    { fbPosts :: [FbPost]
    , paging :: FbPaging
    } deriving (Show, Generic, ToJSON)

instance FromJSON FbPosts where
    parseJSON (Object v) =
        FbPosts <$> v .: "data"
                <*> v .: "paging"
    parseJSON _ = mzero

fbFields = "link,message,description,caption,story,created_time,picture,from,object_id,type"

data FbPost = FbPost
    { fbId :: String
    , fbCreatedTime :: String
    , fbStory :: Maybe String
    , fbMessage :: Maybe String
    , fbPicture :: Maybe String
    , fbCaption :: Maybe String
    , fbFrom :: FbFrom
    , fbLink :: Maybe String
    , fbObjectID :: Maybe String
    , fbType :: String
    } deriving (Show, Generic, ToJSON)

instance FromJSON FbPost where
    parseJSON (Object v) =
        FbPost <$> v .: "id"
               <*> v .: "created_time"
               <*> v .:? "story"
               <*> v .:? "message"
               <*> v .:? "picture"
               <*> v .:? "caption"
               <*> v .: "from"
               <*> v .:? "link"
               <*> v .:? "object_id"
               <*> v .: "type"
    parseJSON _ = mzero

data FbFrom = FbFrom
    { fromName :: String
    , fromId :: String
    } deriving (Show, Generic, ToJSON)

instance FromJSON FbFrom where
    parseJSON (Object v) =
        FbFrom <$> v .: "name"
               <*> v .: "id"
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

data FbImages = FbImages
    { images :: [FbImage]
    } deriving (Show, Generic, ToJSON, FromJSON)

data FbImage = FbImage
    { height :: Int
    , source :: String
    , width :: Int
    } deriving (Show, Generic, ToJSON, FromJSON)

routes :: ScottyM ()
routes = do
    get "/v1/posts" $ do
        fbId <- param "fb_id"
        fbToken <- param "fb_token"
        postsRes <- liftIO(getPosts fbId fbToken)
        let res = fst postsRes
        let code = snd postsRes
        let parsedPosts = (parsePosts res)
        postsWithImages <- liftIO(updatePostsImages fbToken parsedPosts)
        status $ mkStatus code (NL8.pack "")
        if code == 200
            then json $ makePostsGeneric postsWithImages fbToken
            else json $ object [ "error" .= (getErrorMessage $ parseError res) ]

getErrorMessage :: FbError -> String
getErrorMessage FbError{Main.error=err} = message err

makePostsGeneric :: FbPosts -> String -> Posts
makePostsGeneric posts fbToken =
    Posts (Prelude.map (makePostGeneric fbToken) (fbPosts posts)) (Main.next $ paging posts)

trimOffsetFromTime :: String -> String
trimOffsetFromTime time = Prelude.take (Prelude.length time - 5) time

-- TODO: Make this asyncronous
updatePostsImages :: String -> FbPosts -> IO(FbPosts)
updatePostsImages fbToken fbPostsWrap = do
    let posts = fbPosts fbPostsWrap
    updatedPosts <- mapM (updatePostImage fbToken) posts
    return fbPostsWrap { fbPosts = updatedPosts }

updatePostImage :: String -> FbPost -> IO (FbPost)
updatePostImage fbToken fbPost = do
    if (fbType fbPost) == "photo"
        then do let id = (fromMaybe "" (fbObjectID fbPost))
                photo <- getFbPhoto id fbToken
                let updatedFbPost = fbPost { fbPicture = (Just photo) }
                return updatedFbPost
        else return fbPost

getFbPhoto :: String -> String -> IO (String)
getFbPhoto objectId fbToken = do
    photos <- getPhotos objectId fbToken
    let res = fst photos
    let code = snd photos
    if code == 200
        then return $ fromMaybe "" (parseBestPhoto res 600)
        else return ""

parseBestPhoto :: String -> Int -> Maybe String
parseBestPhoto photos targetWidth = 
    let p = Data.Aeson.decode $ L8.pack photos :: Maybe FbImages
    in  if isJust p
            then getBestPhoto (fromJust p) targetWidth
            else Nothing

getBestPhoto :: FbImages -> Int -> Maybe String
getBestPhoto photos targetWidth =
    let options = images photos
    in  if Prelude.length options == 0
            then Nothing
            else Just $ source $ Prelude.foldr (getBetterPhoto targetWidth) (Prelude.head options) options

getBetterPhoto :: Int -> FbImage -> FbImage -> FbImage
getBetterPhoto targetWidth first second
    | w1 >= targetWidth && w2 < targetWidth = first
    | w1 < targetWidth && w2 < targetWidth && w1 > w2 = first
    | w1 >= targetWidth && w2 >= targetWidth && w1 < w2 = first
    | otherwise = second
    where w1 = width first
          w2 = width second

makePostGeneric :: String -> FbPost -> Post
makePostGeneric fbToken fbPost = 
    Post { Main.id = fbId fbPost
         , profileimg = ""
         , date = (trimOffsetFromTime $ fbCreatedTime fbPost) ++ "Z"
         , title = (fromMaybe "" (fbStory fbPost))
         , content = (fromMaybe "" (fbMessage fbPost))
         , heroimg = (fromMaybe "" (fbPicture fbPost))
         , author = (fromName (fbFrom fbPost))
         , postlink = (fromMaybe "" (fbLink fbPost))
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

getPhotos :: String -> String -> IO (String, Int)
getPhotos objectId fbToken = do
    initReq <- parseRequest ("https://graph.facebook.com/" ++ objectId ++ "?fields=images")
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fbToken)) ]
                }
    response <- httpLBS req
    let code = getResponseStatusCode response
    return (L8.unpack $ getResponseBody response, code)

getPosts :: String -> String -> IO (String, Int)
getPosts fbId fbToken = do
    initReq <- parseRequest ("https://graph.facebook.com/" ++ fbId ++ "/feed?fields=" ++ fbFields)
    let req = initReq 
                { requestHeaders =
                    [ ("Authorization", NL8.pack ("Bearer " ++ fbToken)) ]
                }
    response <- httpLBS req
    let code = getResponseStatusCode response
    return (L8.unpack $ getResponseBody response, code)

main = do
    putStrLn "Starting Server on port 5000"
    scotty 5000 routes
